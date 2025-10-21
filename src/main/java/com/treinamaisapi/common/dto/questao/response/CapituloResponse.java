package com.treinamaisapi.common.dto.questao.response;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class CapituloResponse {
    private Long id;
    private String nome;
    private String temaNome;
}

