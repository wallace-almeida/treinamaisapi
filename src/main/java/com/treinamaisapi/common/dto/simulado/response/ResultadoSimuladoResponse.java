package com.treinamaisapi.common.dto.simulado.response;

import lombok.Builder;
import lombok.Data;

import java.util.List;

@Builder
@Data
public class ResultadoSimuladoResponse {
    private Long simuladoId;
    private Double pontuacaoFinal;
    private Integer totalQuestoes;
    private Integer totalAcertos;
    private Integer totalErros;
    private List<FeedbackQuestaoResponse> feedbackQuestoes;
}
