package com.treinamaisapi.common.dto.questao.request;

import lombok.Data;

import java.util.List;

@Data
public class EstruturaLoteRequest {
    private List<EstruturaHierarquicaRequest> temas;
}
