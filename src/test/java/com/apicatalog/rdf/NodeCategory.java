package com.apicatalog.rdf;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

final class NodeCategory {

    int objectOccurence;
    int subjectOccurence;
    int graphOccurence;
    
    Set<String> objectPredicate;
    Set<String> subjectPredicate;
    
    public NodeCategory() {
        this.objectOccurence = 0;
        this.subjectOccurence = 0;
        this.graphOccurence = 0;
        this.objectPredicate = new HashSet<>();
        this.subjectPredicate = new HashSet<>();
    }
        
    public void addObject(String predicate) {
        this.objectOccurence++;
        
        if (predicate != null) {
            this.objectPredicate.add(predicate);
        }
    }
    
    public void addSubject(String predicate) {
        this.subjectOccurence++;
        if (predicate != null) {
            this.subjectPredicate.add(predicate);
        }
    }
    
    public void addGraph() {
        this.graphOccurence++;
    }

    @Override
    public int hashCode() {
        return Objects.hash(objectOccurence, subjectOccurence, graphOccurence, subjectPredicate, objectPredicate);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        NodeCategory other = (NodeCategory) obj;
        return objectOccurence == other.objectOccurence 
                && subjectOccurence == other.subjectOccurence
                && graphOccurence == other.graphOccurence
                && subjectPredicate.equals(other.subjectPredicate)
                && objectPredicate.equals(other.objectPredicate)
                ;
    }      
}

