# Distributed-File-System

## Introduction
This project involved building a *distributed file system* with a range of functionality. 
I chose to implement the following functionalities:

 * __Distributed Transparent File Access__
 * __Directory Server__
 * __Locking Server__
 * __Replication__
 * __Caching__
 * __Authentication__
 
 As an extra I also implented 
 * __Configuration Server__
 
 Below I discuss in detail the inner workings of each part of the system. 
 
 ### System Overview
 In order for the system to be trully distributed, each server is its own self contained [Haskell Stack]() project that can be built and run on its own.
 This prevents the system from having a single point of failure and is thus inherently distributed in nature. If any node fails for any reason, another can simply be spun into action, without affecting the system as a whole.
 
I have included a _distributed-file-system.hsfiles_ in the source. This file is a Stack template which generate the boiler plate of the server code that is used in each of the projects. This file allows for rapid and reliable development of further functionality.

#### Api
The project was built with [Servant](). This is a fantastic tool for building type-safe Api's and I cannot recommend it enough. I have refactored out the code for each of the servers _Api's_ into an [Api Package]() (which in itself is a Stack project). This allows the _Api_ to be imported as a package from a GitHub repository and ensure a common _Api_ accross all of the servers. The servers pull in the Api's from a certain commit, so therefore changes to the _Api_ can be made and tested without affecting the current running of the server. This allows the servers to easily interact with eachothers endpoints.

#### Database
I chose to use [PostgreSQL]() with [Persistent]() for my data store. These two tools used togther create a very simple and reliable way to manage and maintain databases. The migrations are taken care behind the scenes and PostgreSQL ensures that all my [ACID]() requirements were satisfied.

#### Configuration
As mentioned above, I also chose to create a _Configuration Server_ for managing the system as a whole. The location of this server is stored in the Api package and is thus known to each of the other servers. 
The purpose of the _Configuration Server_ is manage the network and help maintain the distributed nature. Rather than each server being pre-loaded with an arbritary configuration, on start-up each of the servers make a call to the _Configuration Server_ which reponds with the relevant configuration.


 ## Distributed Transparent File Access
 This is the most basic need of a *distributed file system*. I chose to implement a variant of the NFS file system. Clients have the ability to:
   * Create files
   * Read files
   * Write to existing files
   * Create directories 

When a file server comes online it must make itself known the the various directory servers. In order to do this the file server requests its configuration from the Config Server. 
The file server then repeatedly makes calls to the directory server until it recevives a positive reponse, informing it that the directory server is aware of its presence.
 
Through the use of the __locking server__ clients have unique access when writing a file. No other client can write to this file during the editing process. Once the client saves the file, the lock is released and others can access the write to the file. All of this functionality is hidden from the client to provide a smooth interaction.
 
The File Servers can be viewed as somewhat dumb nodes. Their functionality is mostly managed by other servers. File Servers simply provide endpoints for writing and getting files. For example, the logic behind their load balancing is managed elsewhere. This provides a light-weight, easy to manage node which can replicated numerous times and upon which the system does not heavily rely.
The interaction with this functionality is discussed in more detail in the [Client]() section below.

 ## Directory Server
 The **Directory Server** keeps track of which files are are stored on which nodes. This allows a client to make a call for the listing of all available files. The **Directory Server** also keeps track of which file nodes are currently active. This ensures the server never sends a client the location of an unavailable node.
 
 *New Files*
    1. Check whether the requested filepath is available
    2. If the filepath is available, insert the new file and save it to the appropriate nodes (discussed in replication)
 
 *Writing Files*
    1. Request an update to the appropriate file nodes
 
 *Reading Files*
    1. Send the files contents for read-only access.
 
  ## Locking Server
 The system ensures that a client has exclusive access when writing a file by using a locking server to maintain a database of currently locked files. This prevents write conflicts from happening. To ensure a file is not locked indefintely, there is also a timeout.
 
 Aside from this there is a seperate locking functionality available. Clients can use this server to gain exclusive access to both files and directories. This allows clients who are repeatyedly working on the same files to lock these and ensure the they can only be edited by themselves. There is also a timeout function on this.
 
 ## Replication
In a _distributed system_ it is to be assumed there will be downtime for certain nodes. To prevent this from causing issues to file access, the system implements replication on files. All files are stored to multiple nodes and this information is managed by the directory server. Load balancing of the file servers is also managed. An implementation of the _Least Recently Used_ algorithm maintains a balance accross the system.

When a client requests a file they receive a list of available nodes which contain the file. This information is temporarily stored by the client. The client then makes a re
 
 ## Caching
 * __Complete__
  
 ## Client
 * __Complete__

 ### Bonus: 
 * Security
 * Transactions
  
