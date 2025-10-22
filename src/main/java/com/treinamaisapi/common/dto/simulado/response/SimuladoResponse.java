package com.treinamaisapi.common.dto.simulado.response;

import com.treinamaisapi.entity.questoes_respondida.QuestaoSimulado;
import com.treinamaisapi.entity.simulado.Simulado;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;


@Builder
@Data
public class SimuladoResponse {
    private Long id;
    private String titulo;
    private LocalDateTime dataCriacao;
    private Integer quantidadeQuestoes;
    private Integer tempoDuracao;
    private Double pontuacaoFinal;
    private String banca;
    private String nivelDificuldade;
    private List<QuestaoResumoResponse> questoes;

    public static SimuladoResponse fromEntity(Simulado simulado, List<QuestaoSimulado> questaoSimulados) {
        List<QuestaoResumoResponse> questoes = questaoSimulados.stream()
                .map(q -> new QuestaoResumoResponse(
                        q.getQuestao().getId(),
                        q.getQuestao().getEnunciado(),
                        q.getQuestao().getNivelDificuldade().name(),
                        q.getQuestao().getBanca()
                ))
                .collect(Collectors.toList());

        return SimuladoResponse.builder()
                .id(simulado.getId())
                .titulo(simulado.getTitulo())
                .dataCriacao(simulado.getDataCriacao())
                .quantidadeQuestoes(simulado.getQuantidadeQuestoes())
                .tempoDuracao(simulado.getTempoDuracao())
                .pontuacaoFinal(simulado.getPontuacaoFinal())
                .banca(simulado.getBanca())
                .nivelDificuldade(simulado.getNivelDificuldade())
                .questoes(questoes)
                .build();
    }


}

