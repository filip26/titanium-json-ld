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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

final class NodeClassifier {

    private final Map<String, NodeCategory> categories = new HashMap<>();

    protected NodeClassifier() {
    }

    protected final void add(RdfNQuad nquad) {

        if (nquad.getSubject().isBlankNode()) {
            addSubject(nquad.getSubject().toString(), nquad.getObject().isIRI() ? nquad.getObject().toString() : null);
        }
        if (nquad.getObject().isBlankNode()) {
            addObject(nquad.getObject().toString(), nquad.getSubject().isIRI() ? nquad.getSubject().toString() : null);
        }
        if (nquad.getGraphName().filter(RdfValue::isBlankNode).isPresent()) {
            addGraph(nquad.getGraphName().get().toString());
        }
    }

    private void addSubject(String label, String object) {
        categories.computeIfAbsent(label, x -> new NodeCategory()).addSubject(object);
    }

    private void addObject(String label, String subject) {
        categories.computeIfAbsent(label, x -> new NodeCategory()).addObject(subject);
    }

    private void addGraph(String label) {
        categories.computeIfAbsent(label, x -> new NodeCategory()).addGraph();
    }

    public int size() {
        return categories.size();
    }

    public Map<NodeCategory, List<String>> reduce() {

        Map<NodeCategory, List<String>> reduced = new HashMap<>();

        for (Entry<String, NodeCategory> entry : categories.entrySet()) {

            List<String> list = reduced.get(entry.getValue());

            if (list == null) {
                list = new ArrayList<>();
                reduced.put(entry.getValue(), list);
            }

            list.add(entry.getKey());
        }
        return reduced;
    }
}
