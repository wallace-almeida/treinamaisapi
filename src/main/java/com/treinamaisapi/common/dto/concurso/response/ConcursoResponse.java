package com.treinamaisapi.common.dto.concurso.response;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;
import java.util.List;

@Data
@Builder
public class ConcursoResponse {
    private Long id;
    private String nome;
    private String descricao;
    private LocalDate dataProva;
    private List<String> pacotes; // nomes dos pacotes vinculados
}
