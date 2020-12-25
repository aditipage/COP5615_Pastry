#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit" 
#time "on"

open System
open Akka
open System.Collections.Generic
open Akka.Actor
open Akka.Configuration
open Akka.FSharp

open Akka.FSharp
open Akka.Actor

open System
open Akka
open System.Collections.Generic
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
let system = System.create "system" (Configuration.defaultConfig())
let random = new System.Random()
open System.Security.Cryptography
open System.Text
let args = fsi.CommandLineArgs
let n = (int) args.[1]
let mutable nActors = 0
if n > 10 then
    nActors <- (int)((80 * n)/100)
else
    nActors <- n

//let nActors = 1000
let numRequests = (int) args.[2]
let mutable count = 0
let mutable value = 0

let new_nodes = n - nActors

let leafSetSize = 16
let mutable totalList = []
let mutable nodeIdsSet = Set.empty
let mutable smallerLeafNodeMap:Map<string,Set<string>> = Map.empty
let mutable largerLeafNodeMap:Map<string,Set<string>> = Map.empty
let mutable finalHopMap:Map<string,int> = Map.empty
let mutable leafNodeMap:Map<string,Set<string>> = Map.empty
let mutable MapOfRefandIdMap:Map<string,IActorRef> = Map.empty
let mutable invertedMapofRefandIdMap: Map<IActorRef,string> = Map.empty
let mutable matchingMap : Map<string,string[,]> = Map.empty
let mutable nodeIdsList = Array.create (nActors) ""
let mutable childRefs = []
let mutable totalHops = 0
let mutable currentNode = ""
let mutable allReq = 0

let map = Map.empty.Add('A',10).Add('B',11).Add('C',12).Add('D',13).Add('E',14).Add('F',15).Add('0',0).Add('1',1).Add('2',2).Add('3',3).Add('4',4).Add('5',5).Add('6',6).Add('7',7).Add('8',8).Add('9',9)

type allInfo = struct
    val lowerLeafSet: Set<string>
    val upperLeafSet: Set<string>
    val nodeId: string
    val myRoutingTable: string[,]

    new (lset,uset,nodeID,rt)=
        {lowerLeafSet = lset;upperLeafSet = uset;nodeId=nodeID;myRoutingTable = rt}
end
    
type message = 
    | IndexandHash of string*string
    | Print of string*string
    | ComputeNeighours of string
    | GenerateRoutingTable of string
    | RouteMsg of string*int
    | RouteKey of string*Map<int,allInfo>*int
    | CollectStateTables of Map<int,allInfo>
    | UpdateState of string
    | AddOne of int
    | StartRouting

let ByteToHex bytes = 
    bytes
    |> Array.map (fun (x : byte) -> System.String.Format("{0:X2}", x))
    |> String.concat System.String.Empty

let route (nodeId:string)(string2:string[])(routingTable:string[,])=
    
    let string2 = string2|> Array.filter(fun i -> i<>nodeId)
    for i in 0..string2.Length-1 do   
        let mutable k = 0
        while k < nodeId.Length-1 && k <>string2.Length-1 && nodeId.[k] = string2.[i].[k] do
            k <- k + 1
        if routingTable.[k,(map.[string2.[i].[k]])] = "-1" then
            routingTable.[k,(map.[string2.[i].[k]])] <- string2.[i]
        
    matchingMap <-matchingMap.Add(nodeId,routingTable)
    
let findingBitsMatched(nodeId:string)(key:string)(tempSet:Set<string>)(bitMatchingOfKeyAndNodeId:int)(routingTable:string[,])=
    let mutable finalSet = Set.empty 
    let string2 = Array.ofSeq tempSet
    for i in 0..string2.Length-1 do   
        let mutable k = 0
        while k < key.Length-1 && k <>string2.Length-1 && key.[k] = string2.[i].[k] do
            k <- k + 1
        
        if k >=bitMatchingOfKeyAndNodeId then
            finalSet <- finalSet.Add(string2.[i])
    
    finalSet

let findClosestMatch(key:string)(nodeId:string)(tempSet:Set<string>) = 
    let mutable smallerNum = 0 |> uint64
    let mutable largerNum = 0 |> uint64
    let mutable nodeIdKeyDiff = Math.Max(Convert.ToUInt64(key, 16),Convert.ToUInt64(nodeId, 16)) - Math.Min(Convert.ToUInt64(key, 16),Convert.ToUInt64(nodeId, 16))
    let arr = Array.ofSeq tempSet
    let mutable closestNode = nodeId
    
    for i in 0..arr.Length-1 do
        let new_diff = Math.Max(Convert.ToUInt64(key, 16),Convert.ToUInt64(arr.[i], 16)) - Math.Min(Convert.ToUInt64(key, 16),Convert.ToUInt64(arr.[i], 16))
        if (new_diff < nodeIdKeyDiff) then
            nodeIdKeyDiff <- new_diff
            closestNode <- arr.[i]
    closestNode
    

let matchingKeyInLeafSet (key: string) (nodeID: string) =
    let mutable totalLeafSet = Set.union smallerLeafNodeMap.[nodeID] largerLeafNodeMap.[nodeID]
    totalLeafSet <- totalLeafSet.Add(nodeID)
    let mutable totalLeafSetArr = Array.ofSeq totalLeafSet 
    totalLeafSetArr <- Array.sort totalLeafSetArr
    let mutable i = 0
    let mutable smallestNodeID = ""
    let mutable minDifference = UInt64.MaxValue
    while i < totalLeafSetArr.Length do
        let mutable smallerNum = 0 |> uint64
        let mutable largerNum = 0 |> uint64
        if key < totalLeafSetArr.[i] then 
            smallerNum <- Convert.ToUInt64(key, 16)
            largerNum <- Convert.ToUInt64(totalLeafSetArr.[i], 16)
        else
            smallerNum <- Convert.ToUInt64(totalLeafSetArr.[i], 16)
            largerNum <- Convert.ToUInt64(key, 16)

        if (largerNum - smallerNum) < minDifference then
            minDifference <- (largerNum - smallerNum)
            smallestNodeID <- totalLeafSetArr.[i]
        i <- i+1
    smallestNodeID
    
let updateLeafSetofAffectedNodes (nodeId:string) (newNodeId:string) (completeLeafSet: Set<string>) = 
    let tempSet = Set.empty.Add(newNodeId).Add(nodeId)
    let newRange = Set.union tempSet completeLeafSet
    let newRangeArray = Array.ofSeq newRange 
    let sizeOfLeafSet = 16
    let completeLeafArray = Array.ofSeq completeLeafSet
    
    let mutable tempSmallerLeafSet = Set.empty
    let mutable k = 0
    for i in (sizeOfLeafSet/2)..completeLeafArray.Length-1 do
        tempSmallerLeafSet <- Set.empty
        k <- 0
        while (k < sizeOfLeafSet/2 && (i+k)<newRangeArray.Length) do
            
            tempSmallerLeafSet <- tempSmallerLeafSet.Add(newRangeArray.[i+k])
            k <- k+1
        
        if k>= sizeOfLeafSet/2 then
            smallerLeafNodeMap <- smallerLeafNodeMap.Add(completeLeafArray.[i],tempSmallerLeafSet)
    let mutable tempLargerLeafSet = Set.empty
    k <- 0
    for i in 0..((completeLeafArray.Length-1)- (sizeOfLeafSet/2)) do
        tempLargerLeafSet <- Set.empty
        k <- 0
        while k < sizeOfLeafSet/2 do 
            tempLargerLeafSet <- tempLargerLeafSet.Add(newRangeArray.[i+k+1])
            k <- k+1
        if k >= sizeOfLeafSet/2 then 
            largerLeafNodeMap <- largerLeafNodeMap.Add(completeLeafArray.[i],tempLargerLeafSet)
    
        
let checkIfKeyInRange (key: string) (nodeID: string) =
     
     let mutable minInSmaller = ""
     let mutable maxInSmaller = ""
     let mutable minInLarger = ""
     let mutable maxInLarger = ""

     if smallerLeafNodeMap.[nodeID].IsEmpty then
        minInSmaller <- nodeID
        maxInSmaller <- nodeID
     else 
        minInSmaller <- smallerLeafNodeMap.[nodeID].MinimumElement
        maxInSmaller <- smallerLeafNodeMap.[nodeID].MaximumElement

     if largerLeafNodeMap.[nodeID].IsEmpty then
         minInLarger <- nodeID
         maxInLarger <- nodeID
     else 
        minInLarger <- largerLeafNodeMap.[nodeID].MinimumElement
        maxInLarger <-largerLeafNodeMap.[nodeID].MaximumElement

     let mutable inRange = false
     if(key > minInSmaller && key < maxInLarger) then
        inRange <- true
     inRange   

let changeLeafSet(nodeId:string)(newNodeID:string) =
    let completeLeafSet = Set.union smallerLeafNodeMap.[nodeId] largerLeafNodeMap.[nodeId]
    let completeLeafArray = Array.ofSeq completeLeafSet
    
    if (newNodeID > nodeId) then
        // to be added in largerLeafSet
        let mutable updatedLargerLeafSet = largerLeafNodeMap.[nodeId]
        if not (updatedLargerLeafSet.IsEmpty || updatedLargerLeafSet.Count<8) then
            updatedLargerLeafSet <- updatedLargerLeafSet.Remove(updatedLargerLeafSet.MaximumElement)
        updatedLargerLeafSet <- updatedLargerLeafSet.Add(newNodeID)
        largerLeafNodeMap <- largerLeafNodeMap.Add(newNodeID,updatedLargerLeafSet)
    
    else
        // to be added in smaller leaf set
        let mutable updatedSmallerLeafSet = smallerLeafNodeMap.[nodeId]
        if not (updatedSmallerLeafSet.IsEmpty || updatedSmallerLeafSet.Count<8) then
            updatedSmallerLeafSet <- updatedSmallerLeafSet.Remove(updatedSmallerLeafSet.MinimumElement)
        updatedSmallerLeafSet <- updatedSmallerLeafSet.Add(newNodeID)
        smallerLeafNodeMap <- smallerLeafNodeMap.Add(newNodeID,updatedSmallerLeafSet)

    updateLeafSetofAffectedNodes nodeId newNodeID completeLeafSet

    
let updateMethod (nodeId:string)(newNodeID:string)(routingTable:string[,]) =
    //perform bit matching and update routing table 
    //update leaf set (upper or lower)
    let mutable bitMatchingOfKeyAndNodeId =0
    let mutable flag = true
    let mutable bitsMatchedCurrent = 0    
               
    while bitMatchingOfKeyAndNodeId<nodeId.Length && nodeId.[bitMatchingOfKeyAndNodeId] = newNodeID.[bitMatchingOfKeyAndNodeId] do
        bitMatchingOfKeyAndNodeId <- bitMatchingOfKeyAndNodeId + 1
        flag<-false
    
    if not flag then
        bitsMatchedCurrent <- bitMatchingOfKeyAndNodeId

    if routingTable.[bitsMatchedCurrent,(map.[newNodeID.[bitsMatchedCurrent]])] = "-1" then
        routingTable.[bitsMatchedCurrent,(map.[newNodeID.[bitsMatchedCurrent]])] <- newNodeID
        matchingMap <-matchingMap.Add(nodeId,routingTable)

    if ((checkIfKeyInRange newNodeID nodeId) || smallerLeafNodeMap.[nodeId].Count < 8 || largerLeafNodeMap.[nodeId].Count < 8) then
        changeLeafSet nodeId newNodeID

let printBoss(mailbox: Actor<_>)=
    let mutable index = 0
    let rec loop() = actor {
        let! msg = mailbox.Receive()
        match msg with
        | Print (x,command) ->
            if command.Equals "Static" then
                
                nodeIdsSet <- nodeIdsSet.Add(x)
                MapOfRefandIdMap <- MapOfRefandIdMap.Add(x,mailbox.Sender())
                nodeIdsList.[index] <- x
                smallerLeafNodeMap <- smallerLeafNodeMap.Add(x,Set.empty)
                largerLeafNodeMap <- largerLeafNodeMap.Add(x,Set.empty)
                index <- index + 1
                totalList <- [x] |> List.append totalList
                if index = nActors then
                    mailbox.Self <! ComputeNeighours (x)
                    for i in 0 .. nodeIdsList.Length-1 do
                        MapOfRefandIdMap.[nodeIdsList.[i]] <! GenerateRoutingTable nodeIdsList.[i]

            elif command.Equals "Join" then
                MapOfRefandIdMap <- MapOfRefandIdMap.Add(x,mailbox.Sender())
                smallerLeafNodeMap <- smallerLeafNodeMap.Add(x,Set.empty)
                totalList <- [x] |> List.append totalList
                largerLeafNodeMap <- largerLeafNodeMap.Add(x,Set.empty)
                invertedMapofRefandIdMap <- invertedMapofRefandIdMap.Add(mailbox.Sender(),x)

        | AddOne x ->
            totalHops <- totalHops + x
            allReq <- allReq + 1
            
        | ComputeNeighours (nodeId) ->
            nodeIdsList <- Array.sort nodeIdsList             
            for i in 0 .. nActors-1 do
                for j in 1..leafSetSize/2 do
                    if(i-j>=0) then
                        smallerLeafNodeMap <- smallerLeafNodeMap.Add(nodeIdsList.[i],smallerLeafNodeMap.[nodeIdsList.[i]].Add(nodeIdsList.[(i-j)]))
                for j in 1..leafSetSize/2 do
                    if(i+j<= nActors-1) then
                        largerLeafNodeMap <- largerLeafNodeMap.Add(nodeIdsList.[i],largerLeafNodeMap.[nodeIdsList.[i]].Add(nodeIdsList.[(i+j)]))
                        
        return! loop()
    }   
    loop()
let boss = spawn system "manager" printBoss

let child (mailbox: Actor<_>)=
    let mutable nodeId = ""
    let mutable stateMap =Map.empty
    let mutable UnivSet = Set.empty
    let mutable routingTable = 
        [| for i in 0..(8) do
            yield [|for i in 0..15 do yield "-1"|]
        |] |> array2D

    let rec loop() = actor {
        let! msg = mailbox.Receive()   
        match msg with 
        | IndexandHash (x,command) ->
            let y = SHA256.Create()
            let b = y.ComputeHash(System.Text.Encoding.ASCII.GetBytes x)
            
            nodeId <-  ByteToHex b
            nodeId <- nodeId.[0..8]
           
            boss<!Print (nodeId,command) 
            matchingMap <- matchingMap.Add(nodeId,routingTable)
            
        | GenerateRoutingTable x ->
            route x nodeIdsList routingTable
        
        | CollectStateTables infoMap ->
            stateMap <- infoMap
            let mutable bitsMatchedPrev = -1
            let mutable bitsMatchedCurrent = 0
            for i in 0..stateMap.Count-1 do
                let senderId = stateMap.[i].nodeId
                let mutable bitMatchingOfKeyAndNodeId =0
                
                while bitMatchingOfKeyAndNodeId< nodeId.Length-1 && nodeId.[bitMatchingOfKeyAndNodeId] = senderId.[bitMatchingOfKeyAndNodeId] do
                    bitMatchingOfKeyAndNodeId <- bitMatchingOfKeyAndNodeId + 1
                   
                bitsMatchedCurrent <- bitMatchingOfKeyAndNodeId
                
                for j =  bitsMatchedPrev+1 to bitsMatchedCurrent do
                    routingTable.[j,*] <- stateMap.[i].myRoutingTable.[j,*]
                    
                bitsMatchedPrev <- bitsMatchedCurrent 
                UnivSet <- UnivSet.Add(senderId)
            
            smallerLeafNodeMap<- smallerLeafNodeMap.Add(nodeId,stateMap.[stateMap.Count-1].lowerLeafSet)
            largerLeafNodeMap<- largerLeafNodeMap.Add(nodeId,stateMap.[stateMap.Count-1].upperLeafSet)           
            UnivSet <- Set.union UnivSet (Set.union smallerLeafNodeMap.[nodeId] largerLeafNodeMap.[nodeId]) 
            for i in 0..8 do 
               
                for j in 0..15 do
                    
                    if not (routingTable.[i,j].Equals "-1") then
                        UnivSet <- UnivSet.Add(routingTable.[i,j])
            
            for senderId in UnivSet do 
                MapOfRefandIdMap.[senderId] <! UpdateState nodeId
            
        | UpdateState newNodeId ->
            updateMethod nodeId newNodeId routingTable
            
        | RouteMsg (key,hops) ->
            let ifKeyInRange = checkIfKeyInRange key nodeId
            if ifKeyInRange then    
                let mutable bestMatchNodeID = matchingKeyInLeafSet key nodeId
                let mutable leafHops = hops
                if bestMatchNodeID <> nodeId then
                    leafHops <- leafHops + 1
                    ()
                boss <! AddOne leafHops
                
            else 
                let mutable bitMatchingOfKeyAndNodeId =0
                let mutable flag = true
                let mutable bitsMatched = 0
                while bitMatchingOfKeyAndNodeId < nodeId.Length-1 && bitMatchingOfKeyAndNodeId<key.Length && nodeId.[bitMatchingOfKeyAndNodeId] = key.[bitMatchingOfKeyAndNodeId] do
                    bitMatchingOfKeyAndNodeId <- bitMatchingOfKeyAndNodeId + 1
                    flag<-false
                if not flag then
                    bitsMatched <- bitMatchingOfKeyAndNodeId

                if not (routingTable.[bitsMatched,(map.[key.[bitsMatched]])].Equals "-1") then
                    let hops = hops + 1
                    MapOfRefandIdMap.[routingTable.[bitsMatched,(map.[key.[bitsMatched]])]]  <! RouteMsg (key,hops)
                else
                    let tempSet = Set.union smallerLeafNodeMap.[nodeId] largerLeafNodeMap.[nodeId]
                    let finalSet = findingBitsMatched nodeId key tempSet bitMatchingOfKeyAndNodeId routingTable
                    let mutable bestMatchNodeId = findClosestMatch key nodeId finalSet
                    if bestMatchNodeId<>nodeId then
                        let hopsNew = hops + 1
                        MapOfRefandIdMap.[bestMatchNodeId] <! RouteMsg (key,hopsNew)

                    else 
                        boss<!AddOne
                        
                        
        | RouteKey(key,infoMap,hops)->
            let mutable infoMap = infoMap
            infoMap <- infoMap.Add(hops,allInfo(smallerLeafNodeMap.[nodeId],largerLeafNodeMap.[nodeId],nodeId,routingTable))    
            let ifKeyInRange = checkIfKeyInRange key nodeId

            if ifKeyInRange then    
                let mutable bestMatchNodeID = matchingKeyInLeafSet key nodeId
                let mutable leafHops = hops
                if bestMatchNodeID <> nodeId then
                    leafHops <- leafHops + 1
                    MapOfRefandIdMap.[bestMatchNodeID]<! RouteKey(key,infoMap,leafHops)
                    ()
                else 
                    MapOfRefandIdMap.[key]<! CollectStateTables infoMap
                
            else 
                let mutable bitMatchingOfKeyAndNodeId =0
                let mutable flag = true
                let mutable bitsMatched = 0
                
                while bitMatchingOfKeyAndNodeId<nodeId.Length && nodeId.[bitMatchingOfKeyAndNodeId] = key.[bitMatchingOfKeyAndNodeId] do
                    bitMatchingOfKeyAndNodeId <- bitMatchingOfKeyAndNodeId + 1
                    flag<-false

                if not flag then
                    bitsMatched <- bitMatchingOfKeyAndNodeId
                
                if not (routingTable.[bitsMatched,(map.[key.[bitsMatched]])].Equals "-1") then    
                    let hops = hops + 1
                    MapOfRefandIdMap.[routingTable.[bitsMatched,(map.[key.[bitsMatched]])]]  <! RouteKey (key,infoMap,hops)
                else
                    let tempSet = Set.union smallerLeafNodeMap.[nodeId] largerLeafNodeMap.[nodeId]
                    let finalSet = findingBitsMatched nodeId key tempSet bitMatchingOfKeyAndNodeId routingTable
                    let mutable bestMatchNodeId = findClosestMatch key nodeId finalSet
                    if bestMatchNodeId<>nodeId then
                        let hopsNew = hops + 1
                        MapOfRefandIdMap.[bestMatchNodeId] <! RouteKey (key,infoMap,hopsNew)    
                    else 
                        MapOfRefandIdMap.[key]<! CollectStateTables infoMap
        | StartRouting ->  
            for j in 1.. numRequests do
                let randomKey = (random.Next(0,nActors-1)).ToString()
                let y = SHA256.Create()
                let b = y.ComputeHash(System.Text.Encoding.ASCII.GetBytes randomKey)        
                let mutable key =  ByteToHex b
                key <- key.[0..8]
                system.Scheduler.ScheduleTellOnce(TimeSpan.FromSeconds(j|>float),mailbox.Self,RouteMsg(key,0))    
        return! loop()
    }   
    loop()
childRefs <- [for i in 1..nActors do yield (spawn system (sprintf "actor%i" i) child)]

let mutable i = 0

while i < nActors do
    value <- i
    childRefs.[i] <! IndexandHash ((value).ToString(),"Static")
    i <- i + 1
Threading.Thread.Sleep(10)
i <-0

let mutable new_children = []

new_children <- [for i in nActors+1..(nActors + new_nodes) do yield (spawn system (sprintf "actor%i" (i)) child)]

while i < new_nodes do 
    value <- nActors + i
    new_children.[i] <! IndexandHash((value).ToString(),"Join")
    i <- i + 1
Threading.Thread.Sleep(2000)

let mutable randomIndex = -1
for i in 0..new_children.Length-1 do
    if nodeIdsList.Length = 1 then
        randomIndex <- random.Next(0,10)
        
    else
        randomIndex <- random.Next(0,nodeIdsList.Length-1)
        
    let new_key = invertedMapofRefandIdMap.[new_children.[i]]
    
    MapOfRefandIdMap.[nodeIdsList.[randomIndex]] <! RouteKey(new_key,Map.empty<int,allInfo>,0)
    Threading.Thread.Sleep(10)
Threading.Thread.Sleep(2000) 
printfn " join finish"    
   

let mutable key = "1F6562590"
let hops = 0

for i in 0..totalList.Length-1 do
    let currentNode = totalList.[i]
    
    MapOfRefandIdMap.[currentNode] <! StartRouting 
    
        

let mutable flag = false
if numRequests > 0 then 
    while not flag do 
        if allReq >= (totalList.Length-1 * numRequests) then
            flag <- true
else
    printfn "Enter numRequests more than 0"
if n = 1 then 
    printfn "Please enter number of nodes greater than 10"

elif n <= 10 then
    printfn "Please enter number of nodes greater than 10"
else 
    printfn "routing complete"
    let numofReq = numRequests|>float
    printfn "Average is %f" ((totalHops|>float)/(((allReq)|>float)))









