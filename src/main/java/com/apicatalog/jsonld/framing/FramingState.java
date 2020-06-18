package com.apicatalog.jsonld.framing;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashMap;
import java.util.Map;

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
    
    private Map<String, Map<String, Boolean>> done;
    
    private Deque<String> parents;
    
    public FramingState() {
        this.done = new HashMap<>();
        this.parents = new ArrayDeque<>();
    }
    
    public FramingState(FramingState state) {
        this.embed = state.embed;
        this.requireAll = state.requireAll;
        this.embedded = state.embedded;
        this.explicitInclusion = state.explicitInclusion;
        this.omitDefault = state.omitDefault;
        this.graphMap = state.graphMap;
        this.graphName = state.graphName;
        this.done = state.done;
        this.parents =  state.parents;
    }
    
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
    
    public boolean isDone(String subject) {
        return done.containsKey(graphName) && done.get(graphName).containsKey(subject);
    }
    
    public void markDone(String subject) {
        done.computeIfAbsent(graphName, x -> new HashMap<>()).put(subject, Boolean.TRUE);
    }

    public boolean isParent(String subject) {
        return parents.contains(graphName + "@@@" +  subject);
    }
    
    public void addParent(String subject) {
        parents.push(graphName + "@@@" + subject); 
    }
    
    public void removeLastParent() {
        parents.pop();
    }

    public void clearDone() {
        done.clear();
    }
}
