package com.treinamaisapi.common.dto.simulado.response;

import com.treinamaisapi.entity.questoes_respondida.QuestaoSimulado;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class QuestaoExecucaoResponse {
    private Long id;
    private String enunciado;
    private String alternativaA;
    private String alternativaB;
    private String alternativaC;
    private String alternativaD;
    private String tema;
    private String capitulo;
    private String subCapitulo;

    public static QuestaoExecucaoResponse fromEntity(QuestaoSimulado qs) {
        return QuestaoExecucaoResponse.builder()
                .id(qs.getQuestao().getId())
                .enunciado(qs.getQuestao().getEnunciado())
                .alternativaA(qs.getQuestao().getAlternativaA())
                .alternativaB(qs.getQuestao().getAlternativaB())
                .alternativaC(qs.getQuestao().getAlternativaC())
                .alternativaD(qs.getQuestao().getAlternativaD())
                .tema(qs.getQuestao().getSubcapitulo().getCapitulo().getTema().getNome())
                .capitulo(qs.getQuestao().getSubcapitulo().getCapitulo().getTema().getNome())
                .subCapitulo(qs.getQuestao().getSubcapitulo().getNome())
                .build();
    }
}
