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
package com.apicatalog.jsonld.expansion;

import java.util.ArrayDeque;
import java.util.Deque;

import com.apicatalog.jsonld.engine.Action;
import com.apicatalog.jsonld.engine.DocumentConsumer;
import com.apicatalog.jsonld.engine.Engine.State;
import com.apicatalog.jsonld.engine.Runtime;

/**
 *
 * @see <a href=
 *      "https://www.w3.org/TR/json-ld11-api/#expansion-algorithm">Expansion
 *      Algorithm</a>
 *
 */
public class ExpansionEngine implements Runtime {

    final Deque<Action> stack = new ArrayDeque<>();

    public ExpansionEngine(Action initial) {

        this.stack.push(initial);
    }

    public State execute() {

//        stack.peek().accept(this);

        return null;
    }

    @Override
    public void push(Action action) {
        // TODO Auto-generated method stub

    }

    @Override
    public void fetch(String uri, DocumentConsumer consumer) {
        // TODO Auto-generated method stub

    }

    @Override
    public <O> void complete(O output) {
        // TODO Auto-generated method stub

    }

    @Override
    public <I> I input() {
        // TODO Auto-generated method stub
        return null;
    }
}