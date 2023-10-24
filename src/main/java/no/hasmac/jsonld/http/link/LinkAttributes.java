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
package no.hasmac.jsonld.http.link;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * A read-only view of a list of {@link LinkAttribute}.
 *
 */
public class LinkAttributes {

    private final Map<String, List<LinkAttribute>> attributes;

    protected LinkAttributes(final Map<String, List<LinkAttribute>> attributes) {
        this.attributes = attributes;
    }

    public List<LinkAttribute> values(final String name) {
        return attributes.containsKey(name)
                    ? Collections.unmodifiableList(attributes.get(name))
                    : Collections.emptyList();
    }

    public List<LinkAttribute> values() {
        return attributes.values().stream().flatMap(Collection::stream).collect(Collectors.toList());
    }

    public Optional<LinkAttribute> firstValue(final String name) {
        return attributes.containsKey(name)
                    ? Optional.of(attributes.get(name).get(0))
                    : Optional.empty();
    }

    public Set<String> names() {
        return attributes.keySet();
    }

    public boolean isEmpty() {
        return attributes.isEmpty();
    }
}
