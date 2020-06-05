package com.apicatalog.xsd;

import java.math.BigDecimal;
import java.text.DecimalFormat;

public final class XsdDouble {

    
    public static final String toString(BigDecimal bigDecimal) {
        
        if (bigDecimal.compareTo(BigDecimal.ZERO) == 0) {
            return "0.0E0";
        }
        
        return new DecimalFormat("0.0E0").format(bigDecimal);        
    }
}
