package com.adontoApi.util;

import java.io.IOException;

import org.springframework.boot.jackson.JsonComponent;
import org.springframework.util.StringUtils;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;

@JsonComponent
public class StringTrimmerSerializer extends JsonSerializer<String> {

	@Override
	public void serialize(String value, JsonGenerator gen, SerializerProvider serializers) throws IOException {

		if (StringUtils.hasText(value)) {
			value = value.trim();
		}
		gen.writeString(value);
	}

}
