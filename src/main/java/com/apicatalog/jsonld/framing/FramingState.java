package com.apicatalog.jsonld.framing;

import com.apicatalog.jsonld.api.JsonLdEmbed;
import com.apicatalog.jsonld.flattening.NodeMap;

public final class FramingState {

    private JsonLdEmbed embed;
    private boolean requireAll;
    private boolean embedded;
    private boolean explicitInclusion;
    private boolean omitDefault;
    
    private String graphName;
    private NodeMap graphMap;
    
    //TODO subject map
    
    public JsonLdEmbed getEmbed() {
        return embed;
    }
    
    public void setEmbed(JsonLdEmbed embed) {
        this.embed = embed;
    }
    
    public boolean isRequireAll() {
        return requireAll;
    }
    
    public void setRequireAll(boolean requireAll) {
        this.requireAll = requireAll;
    }
    
    public boolean isEmbedded() {
        return embedded;
    }
    
    public void setEmbedded(boolean embedded) {
        this.embedded = embedded;
    }
    
    public boolean isExplicitInclusion() {
        return explicitInclusion;
    }
    
    public void setExplicitInclusion(boolean explicitInclusion) {
        this.explicitInclusion = explicitInclusion;
    }
    
    public boolean isOmitDefault() {
        return omitDefault;
    }
    
    public void setOmitDefault(boolean omitDefault) {
        this.omitDefault = omitDefault;
    }

    public String getGraphName() {
        return graphName;
    }

    public void setGraphName(String graphName) {
        this.graphName = graphName;
    }

    public NodeMap getGraphMap() {
        return graphMap;
    }

    public void setGraphMap(NodeMap graphMap) {
        this.graphMap = graphMap;
    }

}
