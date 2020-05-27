package com.apicatalog.jsonld.uri;

import java.net.URI;
import java.util.Objects;

public final class UriRelativizer {

    UriRelativizer() {
    }

    public static String relativize(URI base, String uri) {

        if (base == null) {
            return uri;
        }
        
//        URI target = URI.create(uri);
//        
//        // compare a scheme and an authority
//        if (!Objects.equals(base.getScheme(), target.getScheme())
//                || !Objects.equals(base.getAuthority(), target.getAuthority())
//                ) {
//            
//            return uri;
//        }
        // uri is relative to the base at this point
        
        return uri;
        
//        //base = base.normalize();
//        URI target = URI.create(uri);//.normalize();
//
//        // Split paths into segments
//        String[] bParts = base.getPath().split("\\/");
//        String[] cParts = target.getPath().split("\\/");
//
//        // Discard trailing segment of base path
//        if (bParts.length > 0 && !base.getPath().endsWith("/")) {
//          bParts = Arrays.copyOf(bParts, bParts.length - 1);
//        }
//
//        // Remove common prefix segments
//        int i = 0;
//        while (i < bParts.length && i < cParts.length && bParts[i].equals(cParts[i])) {
//          i++;
//        }
//
//        // Construct the relative path
//        StringBuilder sb = new StringBuilder();
//        for (int j = 0; j < (bParts.length - i); j++) {
//          sb.append("../");
//        }
//        for (int j = i; j < cParts.length; j++) {
//          if (j != i) {
//            sb.append("/");
//          }
//          sb.append(cParts[j]);
//        }

//        return URI.create(sb.toString()).toString();
        
//        return baseUri.relativize(URI.create(target)).toString();
//        
//        //TODO better
//        if (target.startsWith(baseUri.toString())) {
//            return target.substring(baseUri.toString().length());
//        }
//        
//        return target;
    }
}
