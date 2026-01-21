package com.treinamaisapi.service.flashcarddash;

import com.treinamaisapi.common.dto.desempenho.DesempenhoPorMateriaResponse;
import com.treinamaisapi.common.dto.desempenho.DesempenhoUsuarioResponse;
import com.treinamaisapi.common.dto.desempenho.EvolucaoAcertosResponse;
import com.treinamaisapi.common.dto.flashcarddashboard.FlashcardBaralhoResumo;
import com.treinamaisapi.common.dto.flashcarddashboard.FlashcardsDashboardResponse;
import com.treinamaisapi.entity.pontuacao.Pontuacao;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.util.List;

@Service
@RequiredArgsConstructor
public class FlashCardDashBoardService {

    private final BaralhoRepository baralhoRepository;
    private final CartaoRepository cartaoRepository;

    public FlashcardsDashboardResponse dashboard(Long usuarioId) {

        int pendentesHoje = Math.toIntExact(cartaoRepository.contarPendentesHoje(usuarioId));
        List<FlashcardBaralhoResumo> baralhos = baralhoRepository.listarResumo(usuarioId);

        int totalCartoes = baralhos.stream()
                .mapToInt(b -> b.getTotalCartoes().intValue())
                .sum();

        int metaPercentual = calcularPercentualMeta(totalCartoes, pendentesHoje);

        return new FlashcardsDashboardResponse(
                pendentesHoje,
                metaPercentual,
                baralhos
        );
    }


    private int calcularPercentualMeta(int totalCartoes, int pendentesHoje) {
        if (totalCartoes == 0) {
            return 0;
        }

        double perc = ((totalCartoes - pendentesHoje) / (double) totalCartoes) * 100;

        return (int) Math.min(100, Math.max(0, perc));
    }

}

