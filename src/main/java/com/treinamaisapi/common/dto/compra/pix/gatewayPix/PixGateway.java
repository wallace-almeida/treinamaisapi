package com.treinamaisapi.common.dto.compra.pix.gatewayPix;

import com.treinamaisapi.common.dto.compra.pix.response.PixCobrancaResponse;

import java.math.BigDecimal;

public interface PixGateway {

    PixCobrancaResponse criarCobranca(
            Long compraId,
            BigDecimal valor
    );

    void cancelarCobranca(String txId);
}

