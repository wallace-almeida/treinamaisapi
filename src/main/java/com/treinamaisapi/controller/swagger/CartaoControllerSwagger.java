package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.flashcard.baralho.BaralhoResponse;
import com.treinamaisapi.common.dto.flashcard.baralho.CriarBaralhoRequest;
import com.treinamaisapi.common.dto.flashcard.cartao.CartaoRequest;
import com.treinamaisapi.common.dto.flashcard.cartao.CartaoResponse;
import com.treinamaisapi.common.dto.flashcard.cartao.FlashcardEstudoResponse;
import com.treinamaisapi.common.dto.flashcard.cartao.RevisaoPendenteResponse;
import com.treinamaisapi.entity.usuarios.Usuario;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Flash Card", description = " Cartoes do Usuarios ")
public interface CartaoControllerSwagger {


    @PostMapping
    ResponseEntity<CartaoResponse> criar(
            @AuthenticationPrincipal Usuario user,
            @RequestBody CartaoRequest req);


    @GetMapping("/estudo/proximo")
    ResponseEntity<FlashcardEstudoResponse> proximo(
            @AuthenticationPrincipal Usuario user
    );


    @PostMapping("/{id}/responder")
    ResponseEntity<FlashcardEstudoResponse> responder(
            @AuthenticationPrincipal Usuario user,
            @PathVariable Long id,
            @RequestParam int qualidade
    );
}
