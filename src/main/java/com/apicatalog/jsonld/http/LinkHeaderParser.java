package com.apicatalog.jsonld.http;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * 
 * @see <a href="https://tools.ietf.org/html/rfc8288#appendix-B">Appendix B.  Algorithms for Parsing Link Header Fields</a>
 *
 */
public final class LinkHeaderParser {

    private final char[] linkHeader;
    
    private int index;
    
    public LinkHeaderParser(String linkHeader) {
        this.linkHeader = linkHeader.toCharArray();
        this.index = 0;
    }
    

    public Set<Link> parse() {
        
        Set<Link> webLinks = new LinkedHashSet<>();

        while (index < linkHeader.length) {
        
            // skip leading whitespaces
            while (index < linkHeader.length && Character.isSpaceChar(linkHeader[index])) {
                index++;
            }
            
            if (index >= linkHeader.length || linkHeader[index] != '<') {
                return Collections.emptySet();
            }
            
            int targetStringBegin = ++index;
            
            while (index < linkHeader.length && linkHeader[index] != '>') {
                index++;
            }
            
            if (index >= linkHeader.length || linkHeader[index] != '>') {
                return Collections.emptySet();
            }            
            
            String targetString = String.valueOf(linkHeader, targetStringBegin, index - targetStringBegin);

            System.out.println(">>>> link " + targetString);
            
            index++;
            

            
            return webLinks;
        }
        
        return webLinks;
    }
}
