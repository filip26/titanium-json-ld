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
package no.hasmac.jsonld.compaction;

import java.util.Map.Entry;
import java.util.Optional;

import no.hasmac.jsonld.JsonLdError;
import no.hasmac.jsonld.context.ActiveContext;
import no.hasmac.jsonld.context.TermDefinition;
import no.hasmac.jsonld.json.JsonProvider;
import no.hasmac.jsonld.json.JsonUtils;
import no.hasmac.jsonld.lang.DirectionType;
import no.hasmac.jsonld.lang.Keywords;

import jakarta.json.JsonArrayBuilder;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonString;
import jakarta.json.JsonValue;

/**
 *
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#value-compaction">Value Compaction</a>
 *
 */
public final class ValueCompaction {

    // mandatory
    private final ActiveContext activeContext;

    private ValueCompaction(final ActiveContext activeContext) {
        this.activeContext = activeContext;
    }

    public static ValueCompaction with(ActiveContext activeContext) {
        return new ValueCompaction(activeContext);
    }

    public JsonValue compact(final JsonObject value, final String activeProperty) throws JsonLdError {

        // 1.
        JsonValue result = value;

        // 2.
        if (activeContext.getInverseContext() == null) {
            activeContext.createInverseContext();
        }

        final Optional<TermDefinition> activePropertyDefinition = activeContext.getTerm(activeProperty);

        // 4. - 5.
        JsonValue language = null;
        DirectionType direction = null;

        if (activePropertyDefinition.isPresent()) {
            language = activePropertyDefinition.get().getLanguageMapping();
            direction = activePropertyDefinition.get().getDirectionMapping();
        }

        if (language == null) {
            language = activeContext.getDefaultLanguage() != null
                            ? JsonProvider.instance().createValue(activeContext.getDefaultLanguage())
                            : null;
        }

        if (direction == null) {
            direction = activeContext.getDefaultBaseDirection();
        }

        // 6.
        if (value.containsKey(Keywords.ID) &&
                ((value.size() == 1)
                        || (value.size() == 2 && value.containsKey(Keywords.INDEX)))
                ) {

            // 6.1.
            if (activePropertyDefinition
                    .map(TermDefinition::getTypeMapping)
                    .filter(Keywords.ID::equals)
                    .isPresent()
                    ) {

                result = JsonUtils.toJsonValue(activeContext
                                                .uriCompaction()
                                                .compact(value.getString(Keywords.ID)));

            // 6.2.
            } else if (activePropertyDefinition
                            .map(TermDefinition::getTypeMapping)
                            .filter(Keywords.VOCAB::equals)
                            .isPresent()
                            ) {

                result = JsonUtils.toJsonValue(activeContext
                                                .uriCompaction()
                                                .vocab(true)
                                                .compact(value.getString(Keywords.ID)));
            }
        // 7.
        } else if (value.containsKey(Keywords.TYPE)
                    && activePropertyDefinition
                            .map(TermDefinition::getTypeMapping)
                            .filter(d -> JsonUtils.contains(
                                        d,
                                        value.get(Keywords.TYPE))
                                        )
                            .isPresent()
                    ) {

            result = value.get(Keywords.VALUE);

        // 8.
        } else if (activePropertyDefinition
                        .map(TermDefinition::getTypeMapping)
                        .filter(Keywords.NONE::equals)
                        .isPresent()
                        || (value.containsKey(Keywords.TYPE)
                                && (activePropertyDefinition
                                        .map(TermDefinition::getTypeMapping)
                                        .map(d -> !JsonUtils.contains(d, value.get(Keywords.TYPE)))
                                        .orElse(true)
                                        )
                                )
                    ) {

            // 8.1.
            final JsonArrayBuilder types = JsonProvider.instance().createArrayBuilder();

            final JsonValue resultTypes = result.asJsonObject().get(Keywords.TYPE);

            if (JsonUtils.isNotNull(resultTypes)) {
                for (final JsonValue type : JsonUtils.toCollection(resultTypes)) {

                    types.add(activeContext.uriCompaction().vocab(true).compact(((JsonString)type).getString()));
                }

                result = JsonProvider.instance().createObjectBuilder(result.asJsonObject()).add(Keywords.TYPE, types.build()).build();
            }

        // 9.
        } else if (JsonUtils.isNotString(value.get(Keywords.VALUE))) {

            if (!value.containsKey(Keywords.INDEX)
                    || activePropertyDefinition.filter(td -> td.hasContainerMapping(Keywords.INDEX)).isPresent()
                    ) {
                result = value.get(Keywords.VALUE);
            }

        // 10.
        } else if ((((value.containsKey(Keywords.LANGUAGE)
                                && JsonUtils.isString(value.get(Keywords.LANGUAGE))
                                && JsonUtils.isString(language)
                                && (((JsonString)language).getString().equalsIgnoreCase(value.getString(Keywords.LANGUAGE)))
                                )
                        || (JsonUtils.isNull(language)
                                && (!value.containsKey(Keywords.LANGUAGE) || JsonUtils.isNull(value.get(Keywords.LANGUAGE))))
                        )
                        && ((direction != null && direction != DirectionType.NULL
                                && JsonUtils.isString(value.get(Keywords.DIRECTION))
                                && direction == DirectionType.valueOf(value.getString(Keywords.DIRECTION).toUpperCase())
                                )
                                || ((direction == null || direction == DirectionType.NULL)
                                    && (!value.containsKey(Keywords.DIRECTION)
                                    || DirectionType.NULL == DirectionType.valueOf(value.getString(Keywords.DIRECTION).toUpperCase())
                                ))
                                )
                        )
                    && (!value.containsKey(Keywords.INDEX) || activePropertyDefinition.filter(d -> d.hasContainerMapping(Keywords.INDEX)).isPresent())
                ){

                result = value.get(Keywords.VALUE);
        }

        // 11.
        if (JsonUtils.isObject(result)) {

            final JsonObjectBuilder resultBuilder = JsonProvider.instance().createObjectBuilder();

            for (Entry<String, JsonValue> entry : result.asJsonObject().entrySet()) {
                resultBuilder.add(
                                activeContext
                                        .uriCompaction()
                                        .vocab(true)
                                        .compact(entry.getKey()),
                                entry.getValue()
                                );
            }

            result = resultBuilder.build();
        }

        // 12.
        return result;
    }

}
