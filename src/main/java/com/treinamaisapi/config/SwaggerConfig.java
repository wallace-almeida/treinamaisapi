package com.treinamaisapi.config;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.springdoc.core.models.GroupedOpenApi;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SwaggerConfig {

    @Value("${ambiente.nmAmbiente}")
    private String appEnviroment;

    @Value("${app.description}")
    private String appDescription;

    @Bean
    public GroupedOpenApi api() {
        return GroupedOpenApi.builder()
                .group("treinaMaisApi")
                .pathsToMatch("/api/**", "/auth/**")
                .build();
    }

    @Bean
    public OpenAPI customizeOpenAPI() {
        final String securitySchemeName = "bearerAuth";

        return new OpenAPI()
                .info(
                        new Info()
                                .title("TREINA-MAIS-API [" + appEnviroment + "]")
                                .description(appDescription)
                                .version("1.0")
                                .contact(contact())
                )
                .addSecurityItem(new SecurityRequirement().addList(securitySchemeName)) // 🔥 Aplica o Token em todos os endpoints
                .components(new io.swagger.v3.oas.models.Components()
                        .addSecuritySchemes(securitySchemeName,
                                new SecurityScheme()
                                        .name(securitySchemeName)
                                        .type(SecurityScheme.Type.HTTP)
                                        .scheme("bearer")
                                        .bearerFormat("JWT")
                        ));
    }

    private Contact contact() {
        return new Contact()
                .email("treinamaisapp@gmail.com")
                .name("Treina Mais");
    }
}
