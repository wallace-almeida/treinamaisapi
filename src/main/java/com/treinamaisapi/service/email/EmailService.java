package com.treinamaisapi.service.email;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.List;
import java.util.Map;

@Slf4j
@Service
public class EmailService {

    private final WebClient brevoClient;
    private final String apiKey;
    private final String remetente;

    public EmailService(
            WebClient.Builder builder,
            @Value("${brevo.api.key}") String apiKey,
            @Value("${spring.mail.brevo.mail.from}") String remetente
    ) {
        this.brevoClient = builder
                .baseUrl("https://api.brevo.com/v3")
                .build();
        this.apiKey = apiKey;
        this.remetente = remetente;
    }

    public void enviarEmail(String destinatario, String assunto, String texto) {
        // Brevo payload: /v3/smtp/email
        Map<String, Object> body = Map.of(
                "sender", Map.of("email", remetente),
                "to", List.of(Map.of("email", destinatario)),
                "subject", assunto,
                "textContent", texto
        );

        try {
            brevoClient.post()
                    .uri("/smtp/email")
                    .contentType(MediaType.APPLICATION_JSON)
                    .accept(MediaType.APPLICATION_JSON)
                    .header("api-key", apiKey)
                    .bodyValue(body)
                    .retrieve()
                    .toBodilessEntity()
                    .block();

            log.info("Email enviado via Brevo API para {}", destinatario);

        } catch (Exception e) {
            log.error("Falha ao enviar email via Brevo API: {}", e.getMessage(), e);
            throw e;
        }
    }

    public void testarEnvio() {
        enviarEmail(
                "odontotimee@gmail.com",
                "Teste Brevo API",
                "Email enviado com sucesso usando Brevo API + Spring Boot!"
        );
    }
}
