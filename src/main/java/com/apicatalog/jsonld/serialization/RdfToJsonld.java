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
package com.apicatalog.jsonld.serialization;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonValue;

import com.apicatalog.jsonld.api.JsonLdError;
import com.apicatalog.jsonld.api.JsonLdErrorCode;
import com.apicatalog.jsonld.api.JsonLdOptions.RdfDirection;
import com.apicatalog.jsonld.json.JsonUtils;
import com.apicatalog.jsonld.lang.BlankNode;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.jsonld.lang.LanguageTag;
import com.apicatalog.jsonld.lang.Version;
import com.apicatalog.jsonld.uri.UriUtils;
import com.apicatalog.rdf.RdfDataset;
import com.apicatalog.rdf.RdfGraph;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfTriple;
import com.apicatalog.rdf.lang.RdfConstants;

public final class RdfToJsonld {

    // required
    private RdfDataset dataset;
    
    // optional
    private boolean ordered;
    private RdfDirection rdfDirection;
    private boolean useNativeTypes;
    private boolean useRdfType;
    
    private Version processingMode;
    
    // runtime
    private GraphMap graphMap;
    
    
    private Map<String, Map<String, Boolean>> compoundLiteralSubjects;
    private Map<String, Reference> referenceOnce;
    
    private RdfToJsonld(final RdfDataset dataset) {
        this.dataset = dataset;
        
        // default values
        this.ordered = false;
        this.rdfDirection = null;
        this.useNativeTypes = false;
        this.useRdfType = false;
    }
    
    public static final RdfToJsonld with(final RdfDataset dataset) {
        return new RdfToJsonld(dataset);
    }
    
    public RdfToJsonld ordered(boolean ordered) {
        this.ordered = ordered;
        return this;
    }
    
    public RdfToJsonld rdfDirection(RdfDirection rdfDirection) {
        this.rdfDirection = rdfDirection;
        return this;
    }
    
    public RdfToJsonld useNativeTypes(boolean useNativeTypes) {
        this.useNativeTypes = useNativeTypes;
        return this;
    }
    
    public RdfToJsonld useRdfType(boolean useRdfType) {
        this.useRdfType = useRdfType;
        return this;
    }
    
    public RdfToJsonld processingMode(Version processingMode) {
        this.processingMode = processingMode;
        return this;
    }
    
    public JsonArray build() throws JsonLdError {
        
        graphMap = new GraphMap();
        
        // 3.
        referenceOnce = new LinkedHashMap<>();
        
        // 4.
        compoundLiteralSubjects = new LinkedHashMap<>();
        
        // 5.
        step5(Keywords.DEFAULT, dataset.getDefaultGraph());
        
        for (RdfResource graphName : dataset.getGraphNames()) {
            step5(graphName.getValue(), dataset.getGraph(graphName).orElse(null));            
        }
        
        // 6.
        for (String graphName : graphMap.keys()) {
            
            // 6.1.
            if (compoundLiteralSubjects.containsKey(graphName)) {

                for (final String cl : compoundLiteralSubjects.get(graphName).keySet()) {
                           
                    // 6.1.1.
                    final Reference clEntry = referenceOnce.get(cl);
                    
                    if (clEntry == null) {
                        continue;
                    }
                                        
                    // 6.1.5.
                    final Map<String, JsonValue> clNode = graphMap.get(graphName, cl);
                    
                    graphMap.remove(graphName, cl);
                    
                    if (clNode == null) {
                        continue;
                    }
                    
                    JsonArrayBuilder clArray = Json.createArrayBuilder();
                    
                    // 6.1.6.                    
                    for (JsonValue clReference : graphMap.get(clEntry.graphName, clEntry.subject, clEntry.property).asJsonArray()) {
                        
                        if (JsonUtils.isObject(clReference) 
                                && clReference.asJsonObject().containsKey(Keywords.ID)
                                && cl.equals(clReference.asJsonObject().getString(Keywords.ID))
                                ) {

                            JsonObjectBuilder clObject = Json.createObjectBuilder(clReference.asJsonObject());
                            
                            // 6.1.6.1.
                            clObject = clObject.remove(Keywords.ID);
                            
                            JsonValue value = clNode.get(RdfConstants.VALUE);
                            
                            // 6.1.6.2.
                            if (JsonUtils.isArray(value) && value.asJsonArray().size() == 1) {
                                value = value.asJsonArray().get(0);
                            }
                            
                            if (JsonUtils.isObject(value) && value.asJsonObject().containsKey(Keywords.VALUE)) {
                                value = value.asJsonObject().get(Keywords.VALUE);
                            }

                            clObject = clObject.add(Keywords.VALUE, value);

                            // 6.1.6.3.
                            if (clNode.containsKey(RdfConstants.LANGUAGE)) {
                                
                                JsonValue lang = clNode.get(RdfConstants.LANGUAGE);
                                
                                if (JsonUtils.isArray(lang)) {
                                    lang = lang.asJsonArray().get(0);
                                }
                                
                                if (JsonUtils.isObject(lang) && lang.asJsonObject().containsKey(Keywords.VALUE)) {
                                    lang = lang.asJsonObject().get(Keywords.VALUE);
                                }
                                
                                if (JsonUtils.isNotString(lang) || !LanguageTag.isWellFormed(((JsonString)lang).getString())) {
                                    throw new JsonLdError(JsonLdErrorCode.INVALID_LANGUAGE_TAGGED_STRING);
                                }
                                
                                clObject = clObject.add(Keywords.LANGUAGE, lang);
                            }

                            // 6.1.6.4.     
                            if (clNode.containsKey(RdfConstants.DIRECTION)) {
                                
                                JsonValue direction = clNode.get(RdfConstants.DIRECTION);
                                
                                if (JsonUtils.isArray(direction)) {
                                    direction = direction.asJsonArray().get(0);
                                }
                                
                                if (JsonUtils.isObject(direction) && direction.asJsonObject().containsKey(Keywords.VALUE)) {
                                    direction = direction.asJsonObject().get(Keywords.VALUE);
                                }
                                
                                if (JsonUtils.isNotString(direction) 
                                        || (!"ltr".equalsIgnoreCase(((JsonString)direction).getString())
                                            && !"rtl".equalsIgnoreCase(((JsonString)direction).getString()))
                                        ) {
                                    throw new JsonLdError(JsonLdErrorCode.INVALID_BASE_DIRECTION);
                                }
                                
                                clObject = clObject.add(Keywords.DIRECTION, direction);
                            }

                            
                            clArray = clArray.add(clObject);
                        }
                    }
                    graphMap.set(clEntry.graphName, clEntry.subject, clEntry.property, clArray.build());
                }                
            }
            
            // 6.2.
            if (!graphMap.contains(graphName, RdfConstants.NIL)) {
                continue;
            }

            // 6.4.
            for (Reference usage : graphMap.getUsages(graphName, RdfConstants.NIL)) {

                // 6.4.1.
                Map<String, JsonValue> node = graphMap.get(usage.graphName, usage.subject); 
                                
                // 6.4.2.
                List<JsonValue> list = new ArrayList<>();
                List<String> listNodes = new ArrayList<>();
                
                String nodeId = ((JsonString)node.get(Keywords.ID)).getString();

                // 6.4.3.
                while (RdfConstants.REST.equals(usage.property)
                        && BlankNode.isWellFormed(nodeId)
                        && referenceOnce.get(nodeId) != null
                        && node.containsKey(RdfConstants.FIRST)
                        && node.containsKey(RdfConstants.REST)
                        && node.get(RdfConstants.FIRST).asJsonArray().size() == 1
                        && node.get(RdfConstants.REST).asJsonArray().size() == 1
                        && (node.size() == 3
                                || (node.size() == 4 && node.containsKey(Keywords.TYPE)
                                    && node.get(Keywords.TYPE).asJsonArray().size() == 1
                                    && node.get(Keywords.TYPE).asJsonArray().contains(Json.createValue(RdfConstants.LIST))
                                    ))
                        ) {

                    // 6.4.3.1.
                    list.add(node.get(RdfConstants.FIRST).asJsonArray().get(0));
                    
                    // 6.4.3.2.
                    listNodes.add(nodeId);
                    
                    // 6.4.3.3.
                    usage = referenceOnce.get(nodeId);
                    
                    // 6.4.3.4.
                    node = graphMap.get(usage.graphName, usage.subject);

                    if (node == null || !node.containsKey(Keywords.ID)) {
                        break;
                    }
                    
                    nodeId = ((JsonString)node.get(Keywords.ID)).getString();
   
                    // 6.4.3.5.
                    if (UriUtils.isAbsoluteUri(nodeId)) {

                        break;
                    }                    
                }
                

                Collections.reverse(list);
                
                if (graphMap.contains(usage.graphName, usage.subject, usage.property)) {
                
                    JsonArray headArray = graphMap
                                            .get(usage.graphName, usage.subject, usage.property)
                                            .asJsonArray(); 
    
                    JsonObject head = headArray.getJsonObject(usage.valueIndex);
                    
                    JsonArrayBuilder listArray;
                    
                    if (head.containsKey(Keywords.LIST)) {
                        listArray = Json.createArrayBuilder(head.getJsonArray(Keywords.LIST));
                        
                    } else {
                        listArray = Json.createArrayBuilder();
                    }
                    
                    list.forEach(listArray::add);
                    
                    headArray = Json.createArrayBuilder(headArray).set(usage.valueIndex, 
                                        Json.createObjectBuilder(head).remove(Keywords.ID)
                                        .add(Keywords.LIST, listArray)
                                            ).build();
                    
                    graphMap.set(usage.graphName, usage.subject, usage.property, headArray);
                }
                
                // 6.4.7.
                listNodes.forEach(nid -> graphMap.remove(graphName, nid));
            }    
        }
        
        // 7.
        final JsonArrayBuilder result = Json.createArrayBuilder();
        
        // 8.
        final List<String> subjects = new ArrayList<>(graphMap.keys(Keywords.DEFAULT));
        
        if (ordered) {
            Collections.sort(subjects);
        }
        
        for (final String subject : subjects) {
                        
            final Map<String, JsonValue> node = graphMap.get(Keywords.DEFAULT, subject);
        
            // 8.1.
            if (graphMap.contains(subject)) {

                final List<String> keys = new ArrayList<>(graphMap.keys(subject));
                
                if (ordered) {
                    Collections.sort(keys);
                }
                
                final JsonArrayBuilder array = Json.createArrayBuilder();
                
                for (final String key : keys) {
                    
                    final Map<String, JsonValue> entry = graphMap.get(subject, key);
                    
                    if (entry.size() > 1 || !entry.containsKey(Keywords.ID)) {
                        array.add(JsonUtils.toJsonObject(entry));                        
                    }
                }
                
                node.put(Keywords.GRAPH, array.build());                
            }
            
            // 8.2.
            if (node.size() > 1 || !node.containsKey(Keywords.ID)) {            
                result.add(JsonUtils.toJsonObject(node));
            }   
        }
        // 9.
        return result.build();
    }
    
    private void step5(final String graphName, final RdfGraph graph) throws JsonLdError {
                
        // 5.3.
        if (!compoundLiteralSubjects.containsKey(graphName)) {
            compoundLiteralSubjects.put(graphName, new LinkedHashMap<>());
        }

        // 5.4.
        if (!Keywords.DEFAULT.equals(graphName) && !graphMap.contains(Keywords.DEFAULT, graphName)) {
            graphMap.set(Keywords.DEFAULT, graphName, Keywords.ID, Json.createValue(graphName));
        }
        
        // 5.6.
        final Map<String, Boolean> compoundMap = compoundLiteralSubjects.get(graphName);
        
        // 5.7.
        for (final RdfTriple triple : graph.toList()) {
                     
            final String subject = triple.getSubject().toString();
            final String predicate = triple.getPredicate().toString();

            // 5.7.1.
            if (!graphMap.contains(graphName, subject)) {
                graphMap.set(graphName, subject, Keywords.ID, Json.createValue(subject));
            }
                        
            // 5.7.3.
            if (RdfDirection.COMPOUND_LITERAL == rdfDirection 
                    && RdfConstants.DIRECTION.equals(predicate)) {
                
                compoundMap.put(subject, Boolean.TRUE);
            }
            
            // 5.7.4.
            if ((triple.getObject().isBlankNode() || triple.getObject().isIRI())
                    && !graphMap.contains(graphName, triple.getObject().toString())) {
                                
                graphMap.set(graphName, triple.getObject().toString(), Keywords.ID, Json.createValue(triple.getObject().toString()));
            }
            
            // 5.7.5.
            if (!useRdfType && RdfConstants.TYPE.equals(predicate) && !triple.getObject().isLiteral()) {
                
                if (graphMap.contains(graphName, subject, Keywords.TYPE)) {
                    
                    JsonArray types = graphMap.get(graphName, subject, Keywords.TYPE).asJsonArray();
                    
                    graphMap.set(graphName, subject, Keywords.TYPE, Json.createArrayBuilder(types).add(triple.getObject().toString()).build());
                    
                } else {
                    
                    graphMap.set(graphName, subject, Keywords.TYPE, Json.createArrayBuilder().add(triple.getObject().toString()).build());
                }
                
                continue;
            }
            
            // 5.7.6.
            final JsonObject value = 
                        RdfToObject
                            .with(triple.getObject(), rdfDirection, useNativeTypes)
                            .processingMode(processingMode)
                            .build();
            
            int valueIndex = 0;
            
            // 5.7.7.
            if (!graphMap.contains(graphName, subject, predicate)) {
                
                graphMap.set(graphName, subject, predicate, Json.createArrayBuilder().add(value).build());
                valueIndex = 0;
                
            // 5.7.8.
            } else {
                
                JsonArray array = graphMap.get(graphName, subject, predicate).asJsonArray();
                valueIndex = array.size();
                
                if (!array.contains(value)) {
                    graphMap.set(graphName, subject, predicate, Json.createArrayBuilder(array).add(value).build());
                }
            }
            
            // 5.7.9.
            if (triple.getObject().isIRI() && RdfConstants.NIL.equals(triple.getObject().toString())) {

                Reference reference = new Reference();
                reference.graphName = graphName;
                reference.subject = subject;
                reference.property = predicate;
                reference.valueIndex = valueIndex;

                graphMap.addUsage(graphName, triple.getObject().toString(), reference);
            
            // 5.7.10.
            } else if (referenceOnce.containsKey(triple.getObject().toString())) {
                
                referenceOnce.put(triple.getObject().toString(), null);
                
            // 5.7.11.
            } else if (triple.getObject().isBlankNode()) {

                Reference reference = new Reference();
                reference.graphName = graphName;
                reference.subject = subject;
                reference.property = predicate;
                reference.valueIndex = valueIndex;
                
                referenceOnce.put(triple.getObject().toString(), reference);
            }
        }        
    }
    
    protected static class Reference {
        private String graphName;
        private String subject;
        private String property;
        private int valueIndex;
    }
}
