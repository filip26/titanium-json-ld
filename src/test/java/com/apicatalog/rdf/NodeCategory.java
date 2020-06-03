package com.apicatalog.rdf;

import java.util.Objects;

final class NodeCategory {

    int objectOccurence;
    int subjectOccurence;
    
    public NodeCategory() {
        this.objectOccurence = 0;
        this.subjectOccurence = 0;
    }
        
    public void addObject() {
        this.objectOccurence++;
    }
    
    public void addSubject() {
        this.subjectOccurence++;
    }

    @Override
    public int hashCode() {
        return Objects.hash(objectOccurence, subjectOccurence);
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
        return objectOccurence == other.objectOccurence && subjectOccurence == other.subjectOccurence;
    }  
}
