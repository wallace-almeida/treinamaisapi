package com.treinamaisapi.controller.swagger;




import com.treinamaisapi.common.dto.questao.request.TemaRequest;
import com.treinamaisapi.common.dto.questao.response.TemaResponse;
import com.treinamaisapi.common.dto.simulado.filtro.PacoteFiltroSimuladoDTO;
import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.request.RespostaSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.response.ResultadoSimuladoResponse;
import com.treinamaisapi.common.dto.simulado.response.SimuladoExecucaoResponse;
import com.treinamaisapi.common.dto.simulado.response.SimuladoResponse;
import com.treinamaisapi.common.dto.simulado.response.SimuladoResumoResponse;
import com.treinamaisapi.entity.usuarios.Usuario;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Tag(name = "Simulado", description = "Usuarios do sistema")
public interface SimuladoControllerSwagger {


    // Cria e retorna o simulado recém-gerado (EM_ANDAMENTO)
    @PostMapping("/create")
    ResponseEntity<SimuladoExecucaoResponse> criarSimuladoComFiltro(
            @RequestParam Long usuarioId,
            @RequestBody CriarSimuladoRequest request
    );


    @GetMapping("/ativo")
    ResponseEntity<?> buscarSimuladoAtivo(
            @AuthenticationPrincipal Usuario usuario
    );


    @GetMapping("/resumo")
    ResponseEntity<List<SimuladoResumoResponse>> listarResumoSimulados(
            @AuthenticationPrincipal Usuario usuario
    );

    // Envia respostas e finaliza
    @PostMapping("/{simuladoId}/responder")
    ResultadoSimuladoResponse responderSimulado(@PathVariable Long simuladoId,
                                                @RequestBody RespostaSimuladoRequest request);

    // Ver resultado detalhado
    @GetMapping("/{simuladoId}/resultado")
    ResultadoSimuladoResponse verResultado(@PathVariable Long simuladoId);

    @GetMapping("/filtros/{usuarioId}")
    List<PacoteFiltroSimuladoDTO> listarFiltrosSimulado(@PathVariable Long usuarioId);

    @DeleteMapping("/delete/{simuladoId}")
    ResponseEntity<Void> deletarSimulado(
            @PathVariable Long simuladoId,
            @AuthenticationPrincipal Usuario usuario
    );
}
