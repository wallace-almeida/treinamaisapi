package com.treinamaisapi.common.dto.desempenho;

import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class DesempenhoUsuarioResponse {

    // Cabeçalho
    private String nome;
    private Integer nivel;
    private Double xpTotal;
    private String tituloNivel;

    // Resumo geral
    private Long questoesResolvidas;
    private Double taxaAcerto;
    private String tempoEstudo;
    private Long diasAtivos;

    // Gráfico
    private List<EvolucaoAcertosResponse> evolucao;

    // Por matéria
    private List<DesempenhoPorMateriaResponse> porMateria;
}