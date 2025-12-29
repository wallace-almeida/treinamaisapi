package com.treinamaisapi.controller.cartao;

import com.treinamaisapi.common.dto.flashcard.cartao.CartaoRequest;
import com.treinamaisapi.common.dto.flashcard.cartao.CartaoResponse;
import com.treinamaisapi.common.dto.flashcard.cartao.FlashcardEstudoResponse;
import com.treinamaisapi.controller.swagger.CartaoControllerSwagger;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.service.cartao.CartaoService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/cartoes", produces = MediaType.APPLICATION_JSON_VALUE)
public class CartaoController implements CartaoControllerSwagger {

    private final CartaoService service;

    @PostMapping
    @Override
    public ResponseEntity<CartaoResponse> criar(
            @AuthenticationPrincipal Usuario user,
            @RequestBody CartaoRequest req) {
        return ResponseEntity.ok(service.criarManual(user.getId(), req));
    }

    // OK
    @GetMapping("/estudo/proximo")
    @Override
    public ResponseEntity<FlashcardEstudoResponse> proximo(
            @AuthenticationPrincipal Usuario user
    ) {
        return ResponseEntity.ok(service.buscarProximoParaEstudo(user.getId()));
    }



    @PostMapping("/{id}/responder")
    @Override
    public ResponseEntity<FlashcardEstudoResponse> responder(
            @AuthenticationPrincipal Usuario user,
            @PathVariable Long id,
            @RequestParam int qualidade
    ) {
        return ResponseEntity.ok(service.revisar(user.getId(), id, qualidade));
    }



}
