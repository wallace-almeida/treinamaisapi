package com.treinamaisapi.common.dto.questao.request;

import lombok.Data;

import java.util.List;

@Data
public class CapituloLoteRequest {
    private String nome;
    private List<String> subcapitulos;
}
