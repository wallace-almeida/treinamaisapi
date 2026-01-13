package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.questao.request.CapituloRequest;
import com.treinamaisapi.common.dto.questao.response.CapituloResponse;
import com.treinamaisapi.entity.usuarios.Usuario;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

@Tag(name = "Acesso", description = "Aceso do usuarios do sistema")
public interface AcessoControllerSwagger {


    @GetMapping("/pacote/{pacoteId}")
    ResponseEntity<?> temAcesso(@PathVariable Long pacoteId,
                                @AuthenticationPrincipal Usuario usuario);
}
