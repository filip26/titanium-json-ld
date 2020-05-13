package com.apicatalog.jsonld;

/**
 * The {@link JsonLdError} type is used to report processing errors.
 * 
 * @see <a href="https://www.w3.org/TR/json-ld11-api/#jsonlderror">JsonLdError Specification</a>
 * 
 */
public final class JsonLdError extends Throwable {

	private static final long serialVersionUID = -1912600269069309493L;
	
	private final JsonLdErrorCode code;

	public JsonLdError(JsonLdErrorCode code) {
		this.code = code;
	}

	public JsonLdError(JsonLdErrorCode code, String message) {
		super(message);
		this.code = code;
	}

	public JsonLdError(JsonLdErrorCode code, Throwable cause) {
		super(cause);
		this.code = code;
	}

	public JsonLdErrorCode getCode() {
		return code;
	}
}
