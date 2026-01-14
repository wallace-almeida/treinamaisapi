package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.cancelamentoCompra.CancelamentoCompraRequest;
import com.treinamaisapi.common.dto.cancelamentoCompra.CancelamentoCompraResponse;
import com.treinamaisapi.common.dto.concurso.request.ConcursoRequest;
import com.treinamaisapi.common.dto.concurso.response.ConcursoResponse;
import com.treinamaisapi.common.dto.concurso.response.ConcursoStatusResponse;
import com.treinamaisapi.entity.usuarios.Usuario;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Cancelamento Pacote", description = "Cancelamento de pacote do usuario")
public interface CancelamentoPacoteControllerSwagger {


    @PostMapping("/{id}/cancelar")
    ResponseEntity<CancelamentoCompraResponse> cancelar(
            @PathVariable Long id,
            @RequestBody(required = false) CancelamentoCompraRequest request,
            @AuthenticationPrincipal Usuario usuario
    );
}
