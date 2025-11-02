package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.compra.response.CompraResponse;
import com.treinamaisapi.common.dto.pacote.response.PacoteResponse;
import com.treinamaisapi.common.dto.usuario.UsuarioRequest;
import com.treinamaisapi.entity.pacotes.PacoteComprado;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Compra de Pacote", description = "Compra de pacotes de Cursos do usuario")
public interface PacoteCompradoControllerSwagger {


    @PostMapping("/pacote/{pacoteId}/usuario/{usuarioId}")
    CompraResponse comprarPacote(
            @PathVariable Long usuarioId,
            @PathVariable Long pacoteId);

    @PostMapping("/desativar/{id}")
    ResponseEntity<Void> desativarExpirado(@PathVariable Long id);
}
