package com.treinamaisapi.common.dto.compra.pix.marcadoPago.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MpPixPaymentRequest {

    private BigDecimal transaction_amount; // ex: 10.00
    private String payment_method_id;      // "pix"
    private String description;            // "Compra pacote X"
    private String external_reference;     // id da compra na sua base

    private Payer payer;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Payer {
        private String email;
    }
}