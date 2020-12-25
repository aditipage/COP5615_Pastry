README: BONUS
================================================================================================================================

Group Members
================================================================================================================================

Kartik Rode (UFID: 8971-1791)
Aditi Page (UFID: 0427-9968)

What is working
================================================================================================================================

We have successfully implemented node failure that is mentioned in section 3.4 of the Pastry paper. Three arguments are taken as 
input: numNodes numRequests numFailedNodes. The third argument takes the number of failed node in the network.
eg: dotnet fsi --langversion:preview pastry_bonus.fsx 100 20 5

We implement this in the following manner:

1) We have killed/deleted a few nodes in the network after the network has been built succesfully and when routing did not start.
2) So these selected nodes which will be deleted from the entries of other nodes' leaf set and routing table. The network then 
adapts to this and sends it to the next closest node it can find. By getting a small increase in average number of hops, we are 
still able to route the messages.

Obervations:
1) As we kill the active nodes, we encounter dead letters. 
2) The active nodes generally continue routing messages as per the algorithm in Section 2.2. The offset on average number of hops 
   increases by a small fraction as compared to the normal working of Pastry.
3) In some cases, the offset on average number of hops increases significantly.


YouTube Link: https://youtu.be/w72lUHfx8oI
