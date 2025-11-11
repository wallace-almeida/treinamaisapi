package com.treinamaisapi.common.dto.simulado.request;

import lombok.Data;

import java.util.List;

@Data
public class CriarSimuladoRequest {
    private Long concursoId;
    private List<Long> temaIds;
    private List<Long> capituloIds;
    private List<Long> subcapituloIds;

    private List<String> bancas;
    private List<String> niveis;

    private Integer quantidadeQuestoes;
    private Integer tempoDuracao;

    // Comportamento avançado
    private Boolean inteligente;       // escolher baseado em dados
    private Boolean balanceado;        // nivel + assunto
    private Boolean usarFraquezas;     // focar fracos
}