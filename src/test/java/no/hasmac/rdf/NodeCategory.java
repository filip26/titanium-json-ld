/*
 * Copyright 2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package no.hasmac.rdf;

import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

final class NodeCategory {

    private int objectOccurence;
    private int subjectOccurence;
    private int graphOccurence;

    private final Set<String> objects;
    private final Set<String> subjects;

    public NodeCategory() {
        this.objectOccurence = 0;
        this.subjectOccurence = 0;
        this.graphOccurence = 0;
        this.objects = new HashSet<>();
        this.subjects = new HashSet<>();
    }

    public void addObject(String subject) {
        this.objectOccurence++;

        if (subject != null) {
            this.subjects.add(subject);
        }
    }

    public void addSubject(String object) {
        this.subjectOccurence++;

        if (object != null) {
            this.objects.add(object);
        }
    }

    public void addGraph() {
        this.graphOccurence++;
    }

    @Override
    public int hashCode() {
        return Objects.hash(objectOccurence, subjectOccurence, graphOccurence, subjects, objects);
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
                && subjects.equals(other.subjects)
                && objects.equals(other.objects)
                ;
    }
}

