package com.treinamaisapi.service.gamificacao.interfac;

import com.treinamaisapi.entity.simulado.Simulado;
import com.treinamaisapi.entity.usuarios.Usuario;

public interface GamificacaoService {

    void processarConclusaoSimulado(Simulado simulado);

    void processarEstudoFlashcard(
            Usuario usuario,
            Long baralhoId,
            int quantidadeCartoes,
            int tempoMinutos
    );
}
