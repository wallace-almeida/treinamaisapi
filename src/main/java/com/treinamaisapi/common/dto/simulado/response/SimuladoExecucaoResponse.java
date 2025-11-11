package com.treinamaisapi.common.dto.simulado.response;

import com.treinamaisapi.entity.questoes_respondida.QuestaoSimulado;
import com.treinamaisapi.entity.simulado.Simulado;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

@Data
@Builder
public class SimuladoExecucaoResponse {

    private Long id;
    private String titulo;
    private LocalDateTime dataCriacao;
    private Integer tempoDuracao;
    private Integer quantidadeQuestoes;
    private String nivelDificuldade;
    private String banca;
    private String status;

    private List<QuestaoExecucaoResponse> questoes;

    public static SimuladoExecucaoResponse fromEntity(Simulado simulado, List<QuestaoSimulado> questoes) {
        return SimuladoExecucaoResponse.builder()
                .id(simulado.getId())
                .titulo(simulado.getTitulo())
                .dataCriacao(simulado.getDataCriacao())
                .tempoDuracao(simulado.getTempoDuracao())
                .quantidadeQuestoes(simulado.getQuantidadeQuestoes())
                .nivelDificuldade(simulado.getNiveis() != null ? String.join(", ", simulado.getNiveis()) : null)
                .banca(simulado.getBancas() != null ? String.join(", ", simulado.getBancas()) : null)
                .status(simulado.getStatus() != null ? simulado.getStatus().name() : null)
                .questoes(
                        questoes.stream()
                                .map(QuestaoExecucaoResponse::fromEntity)
                                .collect(Collectors.toList())
                )
                .build();
    }

}