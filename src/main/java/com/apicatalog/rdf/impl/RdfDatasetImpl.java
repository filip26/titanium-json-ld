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
package com.apicatalog.rdf.impl;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfTriple;

final class RdfDatasetImpl implements RdfDataset {

    private final Map<RdfResource, RdfGraphImpl> graphs;
    
    private final List<RdfNQuad> nquads;
    
    private final RdfGraphImpl defaultGraph;
    
    protected RdfDatasetImpl() {
        this.graphs = new HashMap<>();
        this.nquads = new LinkedList<>();
        this.defaultGraph = new RdfGraphImpl();
    }
    
    @Override
    public RdfGraph getDefaultGraph() {
        return defaultGraph;
    }
        
    @Override
    public List<RdfNQuad> toList() {
        return nquads;
    }
    
    public RdfDataset add(final RdfNQuad nquad) {

        if (nquad == null) {
            throw new IllegalArgumentException();
        }
        
        final Optional<RdfResource> graphName = nquad.getGraphName(); 
        
        if (graphName.isPresent()) {

            RdfGraphImpl graph = graphs.get(graphName.get());
            
            if (graph == null) {
                
                graph = new RdfGraphImpl();
                graphs.put(graphName.get(), graph);
                graph.add(nquad);
                nquads.add(nquad);
                
            } else if (!graph.contains(nquad)) {
                
                graph.add(nquad);
                nquads.add(nquad);
            }
    
        } else {
            
            // add to default graph
            if (!defaultGraph.contains(nquad)) {
                defaultGraph.add(nquad);
                nquads.add(nquad);
            }
        }
        return this;
    }
    
    @Override
    public Set<RdfResource> getGraphNames() {
        return graphs.keySet();
    }

    @Override
    public Optional<RdfGraph> getGraph(final RdfResource graphName) {
        return Optional.ofNullable(graphs.get(graphName));
    }

    @Override
    public int size() {
        return nquads.size();           
    }

    @Override
    public RdfDataset add(RdfTriple triple) {
        
        RdfNQuad nquad = new RdfNQuadImpl(triple.getSubject(), triple.getPredicate(), triple.getObject(), null);
        
        if (!defaultGraph.contains(nquad)) {
            defaultGraph.add(nquad);
            nquads.add(nquad);
        }
        
        return this;
    }
}
