package com.treinamaisapi.common.dto.compra.pix.gatewayPix;

import com.treinamaisapi.common.dto.cancelamentoCompra.MpRefundResponse;
import com.treinamaisapi.common.dto.compra.pix.response.PixCobrancaResponse;

import java.math.BigDecimal;

public interface PixGateway {

    PixCobrancaResponse criarCobranca(Long compraId, BigDecimal valor, String descricao, String emailPagador);


    void cancelarCobranca(String txId);

    MpPaymentStatusResponse buscarPagamento(String paymentId);

    MpRefundResponse reembolsarPagamento(String paymentId, BigDecimal amountOrNull);
}


