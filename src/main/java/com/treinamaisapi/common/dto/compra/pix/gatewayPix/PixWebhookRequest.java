package com.treinamaisapi.common.dto.compra.pix.gatewayPix;

import lombok.Data;

@Data
public class PixWebhookRequest {

    private String txId;     // identificador da cobrança no gateway
    private String status;   // PAID / CONFIRMED / RECEIVED (depende do gateway)
}
