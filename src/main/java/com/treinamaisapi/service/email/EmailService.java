package com.treinamaisapi.service.email;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.stereotype.Service;

@Service
public class EmailService {

    private final JavaMailSender mailSender;
    private final String remetente;

    public EmailService(
            JavaMailSender mailSender,
            @Value("${spring.mail.brevo.mail.from}") String remetente
    ) {
        this.mailSender = mailSender;
        this.remetente = remetente;
    }

    public void enviarEmail(String destinatario, String assunto, String texto) {
        SimpleMailMessage message = new SimpleMailMessage();
        message.setFrom(remetente);
        message.setTo(destinatario);
        message.setSubject(assunto);
        message.setText(texto);

        mailSender.send(message);

        System.out.println("📧 Email enviado para: " + destinatario);
    }

    public void testarEnvio() {
        enviarEmail(
                "odontotimee@gmail.com",
                "Teste Brevo SMTP",
                "Email enviado com sucesso usando Brevo + Spring Boot!"
        );
    }
}

