package com.treinamaisapi.service.pixGateway;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.treinamaisapi.common.dto.compra.pix.gatewayPix.MpPaymentStatusResponse;
import com.treinamaisapi.common.dto.compra.pix.gatewayPix.PixGateway;
import com.treinamaisapi.common.dto.compra.pix.marcadoPago.request.MpPixPaymentRequest;
import com.treinamaisapi.common.dto.compra.pix.response.PixCobrancaResponse;
import com.treinamaisapi.common.exception.BusinessException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;

@Service
@Slf4j
public class PixGatewayMercadoPago implements PixGateway {

    private static final String MP_PAYMENTS_URL = "https://api.mercadopago.com/v1/payments";

    @Value("${mercadopago.access-token}")
    private String accessToken;

    @Value("${mercadopago.webhook-url}")
    private String webhookUrl;

    private final RestTemplate restTemplate;
    private final ObjectMapper objectMapper = new ObjectMapper();

    public PixGatewayMercadoPago(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    @Override
    public PixCobrancaResponse criarCobranca(Long compraId, BigDecimal valor, String descricao, String emailPagador) {

        String prefix = (accessToken != null)
                ? accessToken.substring(0, Math.min(10, accessToken.length()))
                : "null";
        log.info("MP accessToken prefix={}", prefix);

        try {
            BigDecimal valorArredondado = valor.setScale(2, RoundingMode.HALF_UP);

            MpPixPaymentRequest requestBody = MpPixPaymentRequest.builder()
                    .transaction_amount(valorArredondado)
                    .payment_method_id("pix")
                    .description(descricao)
                    .external_reference(String.valueOf(compraId))
                    .payer(
                            MpPixPaymentRequest.Payer.builder()
                                    .email(emailPagador) // << agora vem do usuário!
                                    .build()
                    )
                    .build();

            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.setBearerAuth(accessToken);

            String idemKey = "payment-pix-compra-" + compraId;
            headers.add("X-Idempotency-Key", idemKey);

            log.info("Criando PAYMENT PIX com idempotency-key={}", idemKey);

            HttpEntity<MpPixPaymentRequest> entity = new HttpEntity<>(requestBody, headers);

            ResponseEntity<String> response = restTemplate.exchange(
                    MP_PAYMENTS_URL,
                    HttpMethod.POST,
                    entity,
                    String.class
            );

            log.info("Resposta bruta Payments PIX: status={} body={}",
                    response.getStatusCode(), response.getBody());

            if (!response.getStatusCode().is2xxSuccessful() || response.getBody() == null) {
                throw new BusinessException("Resposta inválida do Mercado Pago ao criar pagamento PIX");
            }

            JsonNode root = objectMapper.readTree(response.getBody());

            String paymentId = root.path("id").asText();
            String status = root.path("status").asText();
            String statusDetail = root.path("status_detail").asText();

            JsonNode poi = root.path("point_of_interaction").path("transaction_data");
            String qrCode = poi.path("qr_code").asText(null);
            String qrCodeBase64 = poi.path("qr_code_base64").asText(null);
            String ticketUrl = poi.path("ticket_url").asText(null);

            log.info("PAYMENT PIX criado. paymentId={} status={} detail={} ticketUrl={}",
                    paymentId, status, statusDetail, ticketUrl);

            OffsetDateTime expiracaoOffset = OffsetDateTime.now(ZoneOffset.of("-03:00"))
                    .plusMinutes(30);

            LocalDateTime expiracaoLocal = expiracaoOffset.toLocalDateTime();

            return PixCobrancaResponse.builder()
                    .txId(paymentId)
                    .qrCodeBase64(qrCodeBase64)
                    .copiaCola(qrCode)
                    .expiracao(expiracaoLocal) // 👈 agora bate com o tipo do builder
                    .ticketUrl(ticketUrl)
                    .build();

        } catch (HttpClientErrorException e) {
            log.error("Erro HTTP ao criar PAYMENT PIX no Mercado Pago. status={} body={}",
                    e.getStatusCode(), e.getResponseBodyAsString(), e);
            throw new BusinessException("Falha ao criar cobrança PIX no Mercado Pago");
        } catch (RestClientException e) {
            log.error("Erro ao criar PAYMENT PIX no Mercado Pago", e);
            throw new BusinessException("Falha ao criar cobrança PIX no Mercado Pago");
        } catch (Exception e) {
            log.error("Erro inesperado ao tratar resposta de PAYMENT PIX", e);
            throw new BusinessException("Erro ao interpretar a resposta do Mercado Pago para PAYMENT PIX");
        }
    }


    @Override
    public void cancelarCobranca(String txId) {
        log.info("Cancelamento de cobrança PIX ainda não implementado. txId={}", txId);
    }

    @Override
    public MpPaymentStatusResponse buscarPagamento(String paymentId) {
        try {
            String url = MP_PAYMENTS_URL + "/" + paymentId;

            HttpHeaders headers = new HttpHeaders();
            headers.setBearerAuth(accessToken);

            HttpEntity<Void> entity = new HttpEntity<>(headers);

            ResponseEntity<MpPaymentStatusResponse> response = restTemplate.exchange(
                    url,
                    HttpMethod.GET,
                    entity,
                    MpPaymentStatusResponse.class
            );

            log.info("Consulta de pagamento PIX. paymentId={} statusHTTP={}",
                    paymentId, response.getStatusCode());

            return response.getBody();

        } catch (RestClientException e) {
            log.error("Erro ao consultar pagamento PIX no Mercado Pago. paymentId={}", paymentId, e);
            throw new BusinessException("Falha ao consultar pagamento no Mercado Pago");
        }
    }
}
