package com.treinamaisapi.config;

import java.text.SimpleDateFormat;
import java.util.Date;

import org.springdoc.core.GroupedOpenApi;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;

@Configuration
public class SwaggerConfig implements WebMvcConfigurer {

	@Value("${ambiente.nmAmbiente}")
	private String appEnviroment;

	@Value("${app.description}")
	private String appDescription;

	@Bean
	public GroupedOpenApi api() {
		return GroupedOpenApi.builder().group("odontoApi").pathsToMatch("/api/**").build();
	}

	@Bean
	public OpenAPI springShopOpenAPI() {
		SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX");
		String serverDate = sdf.format(new Date());
		return new OpenAPI().info(new Info().title("ODONTO-API[" + appEnviroment + "]")
				.description(appDescription.concat(" - ".concat(serverDate))).version("1.0").contact(contact()));

	}

	private Contact contact() {
		Contact contact = new Contact();
		contact.setEmail("odontotimee@gmail.com");
		contact.setName("Odonto Time");
		contact.setUrl(
				"");
		return contact;
	}

}