package com.apicatalog.jsonld.context;

import java.util.Objects;

class InverseDefinition {

    final String variable;
    final String container;
    final String type;
    final String key;

    public InverseDefinition(final String variable, final String container, final String type, final String key) {
        this.variable = variable;
        this.container = container;
        this.type = type;
        this.key = key;
    }

    public String variable() {
        return variable;
    }

    public String container() {
        return container;
    }

    public String type() {
        return type;
    }

    public String key() {
        return key;
    }

    @Override
    public int hashCode() {
        return Objects.hash(container, key, type, variable);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        InverseDefinition other = (InverseDefinition) obj;
        return Objects.equals(container, other.container)
                && Objects.equals(key, other.key)
                && Objects.equals(type, other.type)
                && Objects.equals(variable, other.variable);
    }
}
