package com.apicatalog.jsonld.custom;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.apicatalog.jsonld.JsonLd;
import com.apicatalog.jsonld.JsonLdException;
import com.apicatalog.jsonld.Options;
import com.apicatalog.tree.io.TreeIOException;
import com.apicatalog.tree.io.jakarta.JakartaAdapter;
import com.apicatalog.tree.io.jakarta.JakartaGenerator;
import com.apicatalog.tree.io.java.JavaAdapter;
import com.apicatalog.tree.io.morph.MapOverlay;

import jakarta.json.Json;
import jakarta.json.JsonObject;

class MapOverlayTest {

    static final JsonObject JSON_OBJECT = Json.createObjectBuilder()
            .add("@context", Json.createObjectBuilder()
                    .add("@vocab", "http://example.com/vocab#"))
            .add("@type", "Person")
            .add("test", Json.createObjectBuilder()
                    .add("memberOf", Json.createObjectBuilder()
                            .add("@type", "Department"))
                    .add("relatedTo", Json.createObjectBuilder()
                            .add("@type", "Person")))
            .build();

    @Test
    void testRemove() throws JsonLdException, TreeIOException {

        var map = MapOverlay.newBuilder(JSON_OBJECT, JakartaAdapter.instance())
                .remove("test")
                .put("x", Json.createValue(10), JakartaAdapter.instance())
                .buildTree();
        
//        assertTrue(!map.keys().contains("test"));
        
        var expanded = JsonLd.expand(map, Options.newOptions());
        
//         var x = new JakartaGenerator(Json.createGenerator(System.out)).node(map.node(), map.adapter());
        System.out.println(expanded);
        System.out.println(map.node());
        System.out.println(map.adapter());
    }

}
