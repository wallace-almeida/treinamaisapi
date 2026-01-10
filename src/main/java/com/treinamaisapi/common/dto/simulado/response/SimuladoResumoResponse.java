package com.treinamaisapi.common.dto.simulado.response;

import com.treinamaisapi.entity.simulado.Simulado;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class SimuladoResumoResponse {

    private Long id;
    private String titulo;
    private LocalDateTime dataCriacao;
    private Integer quantidadeQuestoes;
    private Integer tempoDuracao; // em minutos
    private Double pontuacaoFinal;
    private String status;


    public static SimuladoResumoResponse fromEntity(Simulado simulado) {
        return SimuladoResumoResponse.builder()
                .id(simulado.getId())
                .titulo(simulado.getTitulo())
                .dataCriacao(simulado.getDataCriacao())
                .quantidadeQuestoes(simulado.getQuantidadeQuestoes())
                .tempoDuracao(simulado.getTempoDuracao())
                .pontuacaoFinal(simulado.getPontuacaoFinal())
                .status(simulado.getStatus().name())
                .build();
    }


}
