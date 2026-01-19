package com.treinamaisapi.common.dto.compra.pix.gatewayPix;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true) // 👈 ESSENCIAL
public class PixWebhookRequest {

    // opcional, mas já mapeia certinho o que o MP manda:
    // ex: "payment.created", "payment.updated"
    private String action;

    // tipo do recurso de evento: "payment"
    private String type;

    // id da NOTIFICAÇÃO (não é o payment_id ainda)
    private Long id;

    // dentro de "data" vem o id do pagamento
    private DataNode data;

    @Data
    public static class DataNode {
        // aqui vem o payment_id, ex: "141778397697"
        private String id;
    }
}
