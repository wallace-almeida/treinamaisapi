package com.treinamaisapi.common.dto.desconto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

@Builder
@Data
public class CupomPreviewResponse {
    private boolean valido;
    private String mensagem;
    private BigDecimal precoOriginal;
    private BigDecimal precoFinal;
    private BigDecimal desconto;
}

