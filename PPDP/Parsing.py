import json
from pprint import pprint

global children = []

def iLoss(genVals, data):

    losses = []
    
    for val in genVals:
        v, d = len(getChildren(data[val])), len(getChildren(data))
        loss = (v - 1) / d
        losses.append(loss)
    
    return sum(losses)

def getChildren(data):

    for val in data:
        kids, currVal = data["children"], data[val]
        if val != "children" and kids == [] and currVal not in children:
            children.append(currVal)
        else:
            for i in kids:
                getChildren(i)            

    return children

def generalization(node1, node2, data):

    #find depth of node 1
    #find depth of node 2
    #go up a level and see where they connect    

def distance(file):
    
    distances = []
    children = getChildren(data)
    
    for i in range(len(children)):
        child_i = children[i]
        for x in range(i, len(children)):
            child_x = children[x]
            if (child_x, child_i) not in distances:
                gen = generalization(child_i, child_x)
                dist = iLoss(gen, children)
                distances.append((child_i, child_x, gen, dist))
    
    return distances

def hierarchies(files): #files is a list containing all the json tree files

    with open(files) as f:    
        data = json.load(f)
    
##    for file in files:
##        pprint(distance(file))
        
    return True

hierarchies("language.json")
##hierarchies(["DRUG.json", "language.json", "sample.json"])

