package com.treinamaisapi.scheduler;


import com.treinamaisapi.service.pacoteExpirado.PacoteExpiradoService;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class PacoteExpiracaoScheduler {

    private final PacoteExpiradoService pacoteExpiracaoService;

    @Scheduled(cron = "0 0 3 * * *", zone = "America/Sao_Paulo")
    public void executarExpiracao() {
        try {
            pacoteExpiracaoService.expirarPacotes();
        } catch (Exception e) {
            System.err.println("Erro no scheduler: " + e.getMessage());
        }
    }
}