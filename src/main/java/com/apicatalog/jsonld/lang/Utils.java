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
package com.apicatalog.jsonld.lang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

public final class Utils {

    private Utils() {}

    public static final Collection<String> index(final Collection<String> keys, final boolean ordered) {

        if (keys == null || keys.isEmpty()) {
            return Collections.emptyList();
        }

        if (ordered) {
            final ArrayList<String> sorted = new ArrayList<>(keys);
            Collections.sort(sorted);
            return sorted;
        }

        return keys;
    }
}