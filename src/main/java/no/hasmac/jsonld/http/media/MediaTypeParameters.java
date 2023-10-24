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
package no.hasmac.jsonld.http.media;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public final class MediaTypeParameters {

    protected static final MediaTypeParameters EMPTY = new MediaTypeParameters(Collections.emptyMap());

    private final Map<String, List<String>> parameters;

    protected MediaTypeParameters(final Map<String, List<String>> parameters) {
        this.parameters = parameters;
    }

    public Set<String> names() {
        return parameters.keySet();
    }

    public List<String> values(final String name) {
        return parameters.containsKey(name)
                    ? Collections.unmodifiableList(parameters.get(name))
                    : Collections.emptyList();
    }

    public Optional<String> firstValue(final String name) {
        return parameters.containsKey(name)
                    ? Optional.of(parameters.get(name).get(0))
                    : Optional.empty();
    }

    public boolean isEmpty() {
        return parameters.isEmpty();
    }
}
