package com.treinamaisapi.service.serviceAcess;

import com.treinamaisapi.common.exception.BusinessException;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Component
public class RateLimitService {

    private static final int MAX_TENTATIVAS = 5;
    private static final long TEMPO_JANELA_MS = 5 * 60 * 1000; // 5 minutos

    private final Map<Long, TentativaInfo> tentativasPorUsuario = new ConcurrentHashMap<>();

    public void validar(Long usuarioId) {
        long agora = System.currentTimeMillis();

        TentativaInfo info = tentativasPorUsuario.get(usuarioId);

        // Primeira tentativa ou janela expirada
        if (info == null || agora - info.inicioJanela > TEMPO_JANELA_MS) {
            tentativasPorUsuario.put(usuarioId, new TentativaInfo(1, agora));
            return;
        }

        // Excedeu limite
        if (info.tentativas >= MAX_TENTATIVAS) {
            throw new BusinessException(
                    "Muitas tentativas. Tente novamente em alguns minutos."
            );
        }

        // Incrementa tentativas
        info.tentativas++;
    }

    public void resetar(Long usuarioId) {
        tentativasPorUsuario.remove(usuarioId);
    }

    private static class TentativaInfo {
        int tentativas;
        long inicioJanela;

        TentativaInfo(int tentativas, long inicioJanela) {
            this.tentativas = tentativas;
            this.inicioJanela = inicioJanela;
        }
    }
}

