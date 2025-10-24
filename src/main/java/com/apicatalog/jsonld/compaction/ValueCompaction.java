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
package com.apicatalog.jsonld.compaction;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.apicatalog.jsonld.JsonLdError;
import com.apicatalog.jsonld.context.Context;
import com.apicatalog.jsonld.context.TermDefinition;
import com.apicatalog.jsonld.lang.Direction;
import com.apicatalog.jsonld.lang.Keywords;
import com.apicatalog.tree.io.java.NativeAdapter;

/**
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#value-compaction">Value
 *      Compaction</a>
 *
 */
public final class ValueCompaction {

    public static Object compact(final Context context, final Map<String, ?> value, final String activeProperty) throws JsonLdError {

        // 1.
        Object result = value;

        // 2.
        if (context.getInverseContext() == null) {
            context.createInverseContext();
        }

        final var activePropertyDefinition = context.findTerm(activeProperty);

        // 4. - 5.
        String language = null;
        Direction direction = null;

        if (activePropertyDefinition.isPresent()) {
            language = activePropertyDefinition.get().getLanguageMapping();
            direction = activePropertyDefinition.get().getDirectionMapping();
        }

        if (language == null) {
            language = context.getDefaultLanguage();
        }

        if (direction == null) {
            direction = context.getDefaultBaseDirection();
        }

        // 6.
        if (value.containsKey(Keywords.ID) &&
                ((value.size() == 1)
                        || (value.size() == 2 && value.containsKey(Keywords.INDEX)))) {

            // 6.1.
            if (activePropertyDefinition
                    .map(TermDefinition::getTypeMapping)
                    .filter(Keywords.ID::equals)
                    .isPresent()) {

                result = UriCompaction.compact(context, (String) value.get(Keywords.ID));

                // 6.2.
            } else if (activePropertyDefinition
                    .map(TermDefinition::getTypeMapping)
                    .filter(Keywords.VOCAB::equals)
                    .isPresent()) {

                result = UriCompaction.withVocab(context, (String) value.get(Keywords.ID));
            }
            // 7.
        } else if ((value.get(Keywords.TYPE) instanceof String type
                && activePropertyDefinition
                        .map(TermDefinition::getTypeMapping)
                        .filter(type::equals).isPresent())
                ||
                (value.get(Keywords.TYPE) instanceof Collection<?> types
                        && activePropertyDefinition
                                .map(TermDefinition::getTypeMapping)
                                .filter(types::contains)

//                        .filter(d -> JsonUtils.contains(
//                                d,
//                                value.get(Keywords.TYPE)))
                                .isPresent())) {

            result = value.get(Keywords.VALUE);

            // 8.
        } else if (activePropertyDefinition
                .map(TermDefinition::getTypeMapping)
                .filter(Keywords.NONE::equals)
                .isPresent()
                || (value.get(Keywords.TYPE) instanceof String type
                        && (activePropertyDefinition
                                .map(TermDefinition::getTypeMapping)
                                .map(d -> !type.equals(d))
//                                .map(d -> !JsonUtils.contains(d, value.get(Keywords.TYPE)))
                                .orElse(true)))
                || (value.get(Keywords.TYPE) instanceof Collection<?> types
                        && (activePropertyDefinition
                                .map(TermDefinition::getTypeMapping)
                                .map(d -> !types.contains(d))
//                                .map(d -> !JsonUtils.contains(d, value.get(Keywords.TYPE)))
                                .orElse(true)))) {

            // 8.1.
            final var types = new ArrayList<String>();

            final var resultTypes = value.get(Keywords.TYPE);

            if (resultTypes != null) {

                for (final var type : NativeAdapter.asCollection(resultTypes)) {
                    types.add(UriCompaction.withVocab(context, (String) type));
                }

                var resultMap = new HashMap<String, Object>(value);
                resultMap.put(Keywords.TYPE, types);
                result = resultMap;
            }

            // 9.
        } else if (!(value.get(Keywords.VALUE) instanceof String)) {

            if (!value.containsKey(Keywords.INDEX)
                    || activePropertyDefinition.filter(td -> td.hasContainerMapping(Keywords.INDEX)).isPresent()) {
                result = value.get(Keywords.VALUE);
            }

            // 10.
        } else if ((((value.get(Keywords.LANGUAGE) instanceof String langString
                && language != null
                && (language.equalsIgnoreCase(langString)))
                || ((language == null || Keywords.NULL.equals(language))
                        && ((!(value.get(Keywords.LANGUAGE) instanceof String langString)
                                || Keywords.NULL.equals(langString))
//                                || language == null
//                                || JsonUtils.isNull (value.get(Keywords.LANGUAGE))
                        )))
                && ((direction != null && direction != Direction.NULL
                        && value.get(Keywords.DIRECTION) instanceof String dirString
                        && direction == Direction.valueOf(dirString.toUpperCase()))
                        || ((direction == null || direction == Direction.NULL)
                                && ((!(value.get(Keywords.DIRECTION) instanceof String dirString)
                                        || Direction.NULL == Direction.valueOf(dirString.toUpperCase()))))))
                && (!value.containsKey(Keywords.INDEX)
                        || activePropertyDefinition.filter(d -> d.hasContainerMapping(Keywords.INDEX)).isPresent())) {

            result = value.get(Keywords.VALUE);
        }

        // 11.
        if (result instanceof Map<?, ?> map) {

            final var resultMap = new HashMap<String, Object>();

            for (final var entry : map.entrySet()) {
                resultMap.put(
                        UriCompaction.withVocab(context, (String) entry.getKey()),
                        entry.getValue());
            }
            result = resultMap;
        }

        // 12.
        return result;
    }
}