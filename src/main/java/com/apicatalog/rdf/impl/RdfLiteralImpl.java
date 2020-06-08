package com.apicatalog.rdf.impl;

import java.util.Objects;

import com.apicatalog.rdf.RdfLiteral;
import com.apicatalog.rdf.lang.RdfVocabulary;
import com.apicatalog.xml.XsdVocabulary;

final class RdfLiteralImpl implements RdfLiteral {

    private final String value;
    private final String langTag;
    private final String dataType;

    protected RdfLiteralImpl(String value) {
        this(value, null, null);
    }

    protected RdfLiteralImpl(String value, String langTag, String dataType) {
        this.value = value;
        this.langTag = langTag;
        this.dataType = dataType != null 
                            ? dataType 
                            : (langTag == null ? XsdVocabulary.STRING : RdfVocabulary.LANG_STRING);
    }

    @Override
    public String getValue() {
        return value;
    }

    @Override
    public String getDatatype() {
        return dataType;
    }

    @Override
    public String getLanguage() {
        return langTag;
    }

    @Override
    public int hashCode() {
        return Objects.hash(dataType, langTag, value);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        RdfLiteralImpl other = (RdfLiteralImpl) obj;
        return Objects.equals(dataType, other.dataType) && Objects.equals(langTag, other.langTag)
                && Objects.equals(value, other.value);
    }
    
    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        
        builder.append(value);
        
        if (langTag != null) {
            builder.append('@');
            builder.append(langTag);
            
        } else if (dataType != null) {
            builder.append("^^");
            builder.append(dataType);
        }
        
        return builder.toString();
    }
    
}
