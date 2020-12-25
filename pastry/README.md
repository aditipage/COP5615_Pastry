README
================================================================================================================================

Group Members
================================================================================================================================

Kartik Rode (UFID: 8971-1791)
Aditi Page (UFID: 0427-9968)

What is Working
================================================================================================================================
We have implemented the Pastry API as given in the paper, Pastry:
In our project, we have divided the input of the numNodes such that, if the number of nodes is less than 10, then we build a static 
network. Otherwise, in the case of more than 10 nodes, we have added the nodes in the network dynamically using 'Join'. We have 
divided the number of nodes in an 80-20 ratio. 80% of the nodes form a static network and once this static network is formed, we add 
the remaining 20% nodes using 'Join'. In the static network that is built initially, each of the nodes' routing table is initially 
formed using bitmatching. The routing tables are built using a 2D Array such that the number of bits matched matches the row of the 
2D Array and the corresponding nodes matched are placed in the column according to their (n+1)th digit (where n is the number of bits 
matched). The leaf nodes are built by sorting the lost of the nodes in the static network and adding them into a smallerLeafSet and 
largerLeafSet accordingly. 

The routing table and leafSets are generated as is mentioned in the Pastry paper provided for reference: 
In our static network, when we provide a new key, it is routed to the node which is the numerically closest to it using leafSets and 
Rotuing table. Hops are counted in this process for each of the numRequests. 

We have also successfully implemented 'Join':
'Join' is incorporated when number of nodes is > 10. 
In this case, a slightly different process for populating the routing table and the leaf set is followed. When a new node enters the 
network, it is matched randomly with a new node from the existing network. Prefix matching takes place between the two nodes and with 
the help of routing table we decide which node the new node is to be sent to. As we do this, we send the state information (nodeID, 
routing table, leaf set) which is stored in the data structure that is unique to the new node that is in the process of joining. All 
this state information is used to populate the routing table of the new node. Using this information, we are also able to create the 
new node's leaf set. Once this process is done, we further broadcast the addition of new node in the network through the leafset of 
the nearest neighbour found. We then update all the nodes' leaf sets that are in the nearest node's leaf set since their leaf sets 
will also be affected by the addition of this new node. 



Largest Network for Each Topology and Algorithm
================================================================================================================================

Largest network for which the algorithms work on the topology is 10,000 nodes. Number of cores used increases with the number of 
nodes.

Instructions to run the code?
================================================================================================================================

1)	Unzip the project3.zip
2)	Open file pastry.fsx
3)	Run for any number of nodes (numNodes) upto 10000 and any number of requests (numRequests) using the following command:
	dotnet fsi —langversion:preview project3.fsx numNodes numRequests
	e.g., dotnet fsi —langversion:preview project3.fsx 1000 10

Results
================================================================================================================================

We have used b=4 as the value in this project since the nodeIDs are hexadecimal strings. 
| numNodes      | numRequests     | Expected Avg Hops                 | Actual Avg Hops               |
| --------      | -----------     | -----------------                 | -----------------             |
| 200		| 10		   | 1.911				| 1.882		  	 |
| 300   	| 10 		   | 2.057				| 2.078		  	 |
| 400   	| 10 		   | 2.161				| 2.118		  	 |
| 500   	| 10 		   | 2.241				| 2.275		  	 |
| 600   	| 10 		   | 2.307				| 2.391		  	 |
| 700   	| 10 		   | 2.363				| 2.443		  	 |
| 800   	| 10 		   | 2.411				| 2.491		  	 |
| 900   	| 10 		   | 2.453				| 2.491		  	 |
| 1000   	| 10 		   | 2.491				| 2.500		  	 |
| 1500   	| 10 		   | 2.637				| 2.608	  		 |
| 2000   	| 10 		   | 2.741				| 2.718	  		 |
| 2500   	| 10 		   | 2.822				| 2.778		  	 |
| 3000   	| 10 		   | 2.887				| 2.858	  		 |
| 4000   	| 10 		   | 2.991				| 2.961	  		 |
| 5000   	| 10 		   | 3.072				| 3.050		  	 |
| 6000   	| 10 		   | 3.138				| 3.091	  		 |
| 7000   	| 10 		   | 3.193				| 3.171	  		 |
| 8000   	| 10 		   | 3.241				| 3.212		         |
| 9000   	| 10 		   | 3.284				| 3.234	  		 |
| 10000   	| 10 		   | 3.322				| 3.372	  		 |


We are getting an average offset of upto 0.04.
