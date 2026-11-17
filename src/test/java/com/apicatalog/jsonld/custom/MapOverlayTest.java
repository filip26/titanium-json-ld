package com.apicatalog.jsonld.custom;

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
//FIXMe
//    @Test
//    void testRemove() throws JsonLdException {
//
//        var map = MapOverlay.newBuilder(JSON_OBJECT, JakartaAdapter.instance())
//                .remove("test")
//                .put("x", Json.createValue(10), JakartaAdapter.instance())
//                .buildTree();
//        
////        assertTrue(!map.keys().contains("test"));
//        
//        var expanded = JsonLd.expand(map, Options.newOptions());
//        
////         var x = new JakartaGenerator(Json.createGenerator(System.out)).node(map.node(), map.adapter());
//        System.out.println(expanded);
//        System.out.println(map.node());
//        System.out.println(map.adapter());
//    }

}
