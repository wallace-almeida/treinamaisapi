package com.treinamaisapi.controller.simulado;

import com.treinamaisapi.common.dto.simulado.filtro.PacoteFiltroSimuladoDTO;
import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.request.RespostaSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.response.ResultadoSimuladoResponse;
import com.treinamaisapi.common.dto.simulado.response.SimuladoExecucaoResponse;
import com.treinamaisapi.common.dto.simulado.response.SimuladoResumoResponse;
import com.treinamaisapi.controller.swagger.SimuladoControllerSwagger;
import com.treinamaisapi.entity.usuarios.Usuario;
import com.treinamaisapi.service.simulado.SimuladoService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequiredArgsConstructor
@RestController
@RequestMapping(path = "/api/simulado", produces = MediaType.APPLICATION_JSON_VALUE)
public class SimuladoController implements SimuladoControllerSwagger {

    private final SimuladoService simuladoService;

    // Cria e retorna o simulado recém-gerado (EM_ANDAMENTO)
    @PostMapping("/create")
    @Override
    public ResponseEntity<SimuladoExecucaoResponse> criarSimuladoComFiltro(
            @RequestParam Long usuarioId,
            @RequestBody CriarSimuladoRequest request
    ) {
        return ResponseEntity.ok(simuladoService.criarSimulado(request, usuarioId));
    }


    @GetMapping("/ativo")
    @Override
    public ResponseEntity<?> buscarSimuladoAtivo(
            @AuthenticationPrincipal Usuario usuario
    ) {
        if (usuario == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }

        var simuladoAtivo = simuladoService.buscarSimuladoAtivo(usuario.getId());

        if (simuladoAtivo == null) {
            return ResponseEntity.noContent().build();
        }

        return ResponseEntity.ok(simuladoAtivo);
    }



    @GetMapping("/resumo")
    @Override
    public ResponseEntity<List<SimuladoResumoResponse>> listarResumoSimulados(
            @AuthenticationPrincipal Usuario usuario
    ) {
        // Verifica se o usuário está autenticado
        if (usuario == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }

        // Busca o histórico de simulados do usuário
        List<SimuladoResumoResponse> historico = simuladoService.listarResumoSimulados(usuario.getId());

        // Se não houver histórico, retorna 204 No Content
        if (historico == null || historico.isEmpty()) {
            return ResponseEntity.noContent().build();
        }

        // Retorna a lista de simulados
        return ResponseEntity.ok(historico);
    }



    // Envia respostas e finaliza
    @PostMapping("/{simuladoId}/responder")
    @Override
    public ResultadoSimuladoResponse responderSimulado(@PathVariable Long simuladoId,
                                                       @RequestBody RespostaSimuladoRequest request) {
        return simuladoService.responderSimulado(simuladoId, request);
    }

    // Ver resultado detalhado
    @GetMapping("/{simuladoId}/resultado")
    @Override
    public ResultadoSimuladoResponse verResultado(@PathVariable Long simuladoId) {
        return simuladoService.visualizarResultado(simuladoId);
    }

    // montar  a tela do simulado com base no pacote comprado do usuario

    @GetMapping("/filtros/{usuarioId}")
    @Override
    public List<PacoteFiltroSimuladoDTO> listarFiltrosSimulado(@PathVariable Long usuarioId) {
        return simuladoService.listarFiltrosPorUsuario(usuarioId);
    }

    @DeleteMapping("/delete/{simuladoId}")
    @Override
    public ResponseEntity<Void> deletarSimulado(
            @PathVariable Long simuladoId,
            @AuthenticationPrincipal Usuario usuario
    ) {
        if (usuario == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }

        simuladoService.deletarSimulado(simuladoId, usuario.getId());
        return ResponseEntity.noContent().build();
    }



}
