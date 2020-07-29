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

import java.util.Optional;

import com.apicatalog.rdf.RdfNQuad;
import com.apicatalog.rdf.RdfResource;
import com.apicatalog.rdf.RdfValue;

final class RdfNQuadImpl extends RdfTripleImpl implements RdfNQuad {

    private final RdfResource graphName;
    
    protected RdfNQuadImpl(RdfResource subject, RdfResource predicate, RdfValue object, RdfResource graphName) {
        super(subject, predicate, object);
        this.graphName = graphName;
    }
    
    @Override
    public Optional<RdfResource> getGraphName() {
        return Optional.ofNullable(graphName);
    }
}
