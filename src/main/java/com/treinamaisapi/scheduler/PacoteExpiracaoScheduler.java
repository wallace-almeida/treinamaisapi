package com.treinamaisapi.scheduler;


import com.treinamaisapi.service.pacoteExpirado.PacoteExpiradoService;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class PacoteExpiracaoScheduler {

    private final PacoteExpiradoService pacoteExpiracaoService;

    @Scheduled(fixedRate = 60000) // 1 minuto
    public void executarExpiracao() {
        pacoteExpiracaoService.expirarPacotes();
    }
}
