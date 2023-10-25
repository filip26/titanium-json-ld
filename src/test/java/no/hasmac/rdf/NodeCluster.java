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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

final class NodeCluster {

    private Map<String, Integer> source;

    private String[] target;

    private int[] indices;
    private int[] mapping;

    private int iterator;

    private int permutations;

    public NodeCluster(Map<String, Integer> source, String[] target, int permutations) {
        this.source = source;
        this.target = target;

        this.indices = new int[source.size()];
        this.mapping = new int[source.size()];

        this.permutations = permutations;

        reset();
    }

    public String[] target() {
        return target;
    }

    public static NodeCluster create(Collection<String> source, Collection<String> target) {

        if (source.size() != target.size()) {
            throw new IllegalArgumentException();
        }

        Map<String, Integer> sourceMap = new HashMap<>();
        for (String s : source) {
            sourceMap.put(s, sourceMap.size());
        }

        return new NodeCluster(sourceMap, target.toArray(new String[0]), factorial(source.size()));
    }

    private static int factorial(int number) {

        int result = 1;

        for(int i=1; i <= number; i++){
            result = result * i;
        }
        return result;
    }

    public boolean next() {

        if (indices.length == 1) {
            return true;
        }

        while (iterator < indices.length) {
            if (indices[iterator] < iterator) {
                swap(iterator % 2 == 0 ?  0 : indices[iterator], iterator);
                indices[iterator]++;
                iterator = 0;
                break;
            }
            else {
                indices[iterator] = 0;
                iterator++;
            }
        }

        if (iterator >= indices.length) {
            reset();
            return true;
        }

        return false;
    }

    private void swap(int a, int b) {
        int tmp = mapping[a];
        mapping[a] = mapping[b];
        mapping[b] = tmp;
    }

    private void reset() {
        for (int i=0; i < indices.length; i++) {
            indices[i] = 0;
            mapping[i] = i;
        }
        iterator = 0;
    }

    public String mapping(String label) {
        return target[mapping[source.get(label)]];
    }

    public int permutations() {
        return permutations;
    }
}
