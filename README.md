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
 In order for the system to be trully distributed, each server is its own self contained Haskell Stack project that can be built and run on its own.
 This prevents the system from having a single point of failure and is thus inherently distributed in nature. If any node fails for any reason, another can simply be spun into action, without affecting the system as a whole.
 
I have included a _distributed-file-system.hsfiles_ in the source. This file is a Stack template which generate the boiler plate of the server code that is used in each of the projects. This file allows for rapid and reliable development of further functionality.

#### Api
The project was built with Servant. This is a fantastic tool for building type-safe Api's and I cannot recommend it enough. I have refactored out the code for each of the servers Api's into the Api package. This allows the Api to be imported as a package from a GitHub repository and ensure a common Api accross all of the servers. The servers pull in the Api's from a certain commit, so therefore changes can be made and tested without affecting the current running of the server. This allows the servers to easily interact with eachothers endpoints.

#### Database
I chose to use PostgreSQL with persistent for my data store. These two tools used togther create a very simple and reliable way to manage and maintain databases.

#### Configuration
As mentioned above, I also chose to create a _Configuration Server_ for managing the system as a whole. The location of this server is stored in the Api package and is thus known to each of the other servers. 
The purpose of the _Configuration Server_ is manage the network and help maintain the distributed nature. Rather than each server being pre-loaded with an arbritary configuration, on start-up each of the servers make a call to the _Configuration Server_ which reponds with the relevant configuration.


 ## Distributed Transparent File Access
 This is the most basic need of a *distributed file system*. I chose to implement a variant of the NFS file system. Clients have the ability to:
   * Create files
   * Read files
   * Write to existing files
   * Create directories 
 Through the use of the __locking server__ clients have <unique> access when writing a file. No other client can write to this file during the editing process. Once the client saves the file the lock is released others can access the file. All of this functionality is hidden from the client to provide a smooth interaction.
When a file server comes online it must make itself known the the various directory servers. In order to do this the file server requests its configuration from the Config Server. 
 The file server then repeatedly makes calls to the directory server until it recevives a positive reponse, informing it that the directory server is aware of its presence.

 ## Directory Server
 * __Complete__
 
  ## Locking Server
 * __Complete__
 
 ## Replication
 * __Complete__
 
 ## Caching
 * __Complete__
  
 ## Client
 * __Complete__

 ### Bonus: 
 * Security
 * Transactions
  
