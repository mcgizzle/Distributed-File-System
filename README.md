# Distributed-File-System

## Introduction
This project involved building a *distributed file system* with a range of functionality. This was my first Haskell project of such scale and I learned a lot along the way. I now feel far more equipped in the areas of distributed systems and functional programming. 

I chose to implement the following functionalities:

 * __Distributed Transparent File Access__
 * __Directory Server__
 * __Locking Server__
 * __Replication__
 * __Caching__
 
 As an extra I also implented 
 * __Configuration Server__
 
 Below I discuss in detail the inner workings of each part of the system. 
 
 Building & Running is discussed [here](#buildAndrun).
 
 ### System Overview
 In order for the system to be trully distributed, each server is its own self contained [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) project that can be built and run independently from the rest of the system.
 This prevents the system from having a single point of failure and is thus inherently distributed in nature. If any node fails for any reason, another can simply be spun into action, without affecting the system as a whole. This also allows for funcionality to be added easily and quickly, as the entire system is modular.
 
I have included a [_distributed-file-system.hsfiles_](https://github.com/McGizzle/Distributed-File-System/blob/master/servant.hsfiles) in the source. This file is a [stack template](https://github.com/commercialhaskell/stack-templates) which generate the boiler plate for the server code that is used in each of the projects. This file allows for rapid and reliable development of further functionality.

#### Api
The project was built with [Servant](https://haskell-servant.readthedocs.io/en/stable/). This is a fantastic tool for building type-safe Api's and I cannot recommend it enough. I have refactored out the code for each of the servers _Api's_ into an [Api Package](https://github.com/McGizzle/Distributed-File-System/tree/master/Api) (which in itself is a stack project). This allows the _Api_ (ie. the functionality of the system) to be imported as a package from a GitHub repository and ensure a common _Api_ accross all of the servers. The servers pull in the package from a certain commit, so therefore changes to the _Api_ can be made and tested without affecting the current running of the system. This allows the servers to easily interact with eachothers endpoints.

Most importantly, the *Api* package provides simple a Haskell interface for interacting with the system.

#### Database
I chose to use [PostgreSQL](https://www.postgresql.org/) with [Persistent](https://hackage.haskell.org/package/persistent) as my data store. These two tools used togther create a very smooth and reliable way to manage and maintain a database. The migrations are taken care behind the scenes  with a few lines of code and PostgreSQL ensures that all my [ACID](https://en.wikipedia.org/wiki/ACID) requirements are satisfied. 
The use of these tools ensures that the system is persisted. When the system is shut-down and restarted the same information will remain available.

#### Configuration
As mentioned above, I also chose to create a _Configuration Server_ for managing the system as a whole. The location (host and port) of this server is stored in the Api package and is thus known to each of the other servers. 
The purpose of the _Configuration Server_ is manage the network and help maintain the distributed nature. Rather than each server being pre-loaded with an arbritary configuration, on start-up each of the servers make a call to the _Configuration Server_ which reponds with the relevant configuration.

Links to the source:

[Api and Models](https://github.com/McGizzle/Distributed-File-System/blob/master/Api/src/Api/Config.hs)
 
[Controller](https://github.com/McGizzle/Distributed-File-System/blob/master/Config-Server/src/Controller.hs)


 ## Distributed Transparent File Access
 This is the most basic need of a *distributed file system*. I chose to implement a variant of the NFS file system. Clients have the ability to:
   
   * List Files
   * Create Files
   * Read Files
   * Write to Existing Files (exclusively)
   * Create Directories
   * Delete Files/Directories 

As an example of how the functionality could be extended. I have provided a simple text editor for editing files with the client program. When the client chooses to write to a file, it is opened up in the text editor and on save, the file is sent for update.
   
The functionality of theis part of the system is really a combination of all the other servers and thus will be explained further in each seperate section. Therefore I will mainly discuss the operation of the *file servers* here. 
 
Through the use of the __locking server__ clients have unique access when writing a file (among other abilities discussed below).

Caching ensures that unnesecary transfers of large files do not take place.
 
The __file servers__ (which store the files) can be viewed as somewhat dumb nodes.Their functionality is mostly managed by other servers. __File servers__ simply provide endpoints for writing and reading files. For example, the logic behind their load balancing is managed elsewhere. This provides a light-weight, easy to manage node which can replicated numerous times and upon which the system does not heavily rely. The system was built with a layered approach.

When a __file server__ comes online it must make itself known the the various directory servers. In order to do this the file server requests its configuration from the Configuration Server. 
The __file server__ then places itself in a loop, constantly making calls to its designated file server(s) until it receives an acknowledgement, informing it that the directory server is aware of its presence.

Links to the source:

[Api and Models](https://github.com/McGizzle/Distributed-File-System/blob/master/Api/src/Api/File.hs)
 
[Controller](https://github.com/McGizzle/Distributed-File-System/blob/master/File-Server/src/Server.hs)

The interaction with this functionality is discussed in more detail in the Client section below.

 ## Directory Server
 The __directory server__ keeps track of which files are are stored on which nodes. This allows a client to make a call for the listing of all available files. The **directory server** also keeps track of which file nodes are currently active. This ensures the server never sends a client the location of an unavailable node.
 
 **List Files**
 
    List all currently available files on the system.
   
 **New Files**
 
    Check whether the requested filepath is available
    
    If the filepath is available, insert the new file and save it to the appropriate nodes (discussed in replication)
 
 **Writing Files**
 
    Update all of the appropriate file nodes.
 
 **Reading Files**
 
    Send the files location (on potentially numerous nodes).
  
 **Delete Files**
    
    Check whether the file exists
    
    Propogate the deletion accross all nodes which store the file
  
 Links to the source:
 
 [Api and Models](https://github.com/McGizzle/Distributed-File-System/blob/master/Api/src/Api/Directory.hs)
 
 [Controller](https://github.com/McGizzle/Distributed-File-System/blob/master/Directory-Server/src/Controller.hs)
 
  ## Locking Server
 The system ensures that a client has exclusive access when writing a file by using a __locking server__ to maintain a database of currently locked files. This prevents write conflicts from happening.

When a client requests write-access, behind the scenes the file is locked, it is also automatically unlocked upon the subsequent write.
 
 To ensure a file is not locked indefintely, there is also a timeout. When a client first locks a file the timeout is started. If the lock is not renewed or released before the timeout, then the file will become available to the network again. 
 
 Aside from this there is a seperate locking functionality available. Clients can use this server to gain exclusive access to both files and directories outside of just a single write. This allows clients who are repeatedly working on the same files to lock these and ensure the they can only be edited by themselves. Of course there is also a timeout for this feature.

As an extended implementation I set up a FIFO queue within the __locking server__. 
When a client requests a file that is already locked they are added to a FIFO queue by the locking server. Access to the resource will be released through this FIFO queue. There is timeout implemented in which the user next in the queue must make an access to the file before they loose their place.  

 
 Links to the source:
 
 [Api and Models](https://github.com/McGizzle/Distributed-File-System/blob/master/Api/src/Api/Locking.hs)
 
 [Controller](https://github.com/McGizzle/Distributed-File-System/blob/master/Locking-Server/src/Controller.hs)
 
 ## Replication
In a _distributed system_ it is to be assumed there will be downtime for certain nodes. To prevent this from causing issues to file access, the system implements replication of files. All files are stored to multiple nodes and this information is managed by the directory server. Load balancing of the file servers is also managed. 

An implementation of the _Least Recently Used_ algorithm maintains a balance accross the system. The directory server manages this by storing the information in a database and updating it with each write to the various file nodes.

When a client requests a file they receive a list of available nodes which contain the file. This information is temporarily stored by the client. The client then makes a request to one of theses nodes, if the node is down or takes too long to reply, the client simply tries another of the nodes. 
 
 ## Caching
In a distributed system caching is a difficult topic. 

I chose to imlement two versions of caching, allowing the system administrator to make the choice that best fits their specific needs.

__1. Client checks with server__

This system takes the approach that clients will most likely be accessing the same, yet very large, files and therefore acts accordingly. The client is also designed to be light-weight, and thus has no endpoints of its own.

After creating a new file or writing to an existing, the client saves the files contents to the cache along with a time of write. A _last-write_ field for every file is also maintained by the directory server. 

Before a client requests a file from a file server checks its cache for the file. If the file is there, then the client simply asks the directory server whether its version is in date. The directory server responds appropriately.

This implementation allows the client to be an extremely light-weight service and prevents the unnesecary transfer of unaltered files. 

__2. Server informs the client__

The system takes the viewpoint that the load on the directory server will be heavy and many clients will simply be reading rather than writing files.

In this approach to caching, the directory server maintains a list of clients who have cached files. When a write is made to a file, the server invalidates all of the clients caches.
If a client is currently offline, then the server makes note, and notifies the client on startup.

This version requires a more cumbersome client side application. However, it can reduce load to the system as no requests are made to the system to check whether the cache is valid.

  
## Client
The functionality of the system can imported into projects through the provided Api package. An example of this package in action is found in the __Client__. This package displays all of the systems functionality.
  
  
 <div>
 
  Operation | Command 
  ---|--- 
  List Files  | `ls` 
  Read File   | `read <file path>`
  Create File | `new <file name> <file path>`
  Write File  | `write <file path>`
  Delete File | `delete <file path>`
  Lock File   | `lock <file path>`
  Unlock File | `unlock <file path>`
  Caching     | Done behind the scenes
 
  
  <img style="float: right;" src="https://github.com/McGizzle/Distributed-File-System/blob/master/Client/img/console.png">
 
 </div>
 
  ## Building & Running <a name="buildAndrun"></a>
  
  **Each server must be built using**
  
  `stack build`
  
  **Run the servers**
  
  `stack exec <project name>-exe <port>`
  
  To run this locally, first fire up the servers in the following order:
  
  1. Configuration server
  2. Directory & Locking Servers
  3. File Servers
  
  and finally the client
  
  `stack exec Client`
  

## Authentication

Unfortunately, due to time constraints, I did not get to fully implement authentication. I have left the code that I had so far written as an example of how I planned on proceeding. The system would have implmented a token based Api, with each client being assigned a token with a timeout parameter upon startup. 
