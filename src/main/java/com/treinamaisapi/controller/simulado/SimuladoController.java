package com.treinamaisapi.controller.simulado;

import com.treinamaisapi.common.dto.simulado.filtro.PacoteFiltroSimuladoDTO;
import com.treinamaisapi.common.dto.simulado.request.CriarSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.request.RespostaSimuladoRequest;
import com.treinamaisapi.common.dto.simulado.response.ResultadoSimuladoResponse;
import com.treinamaisapi.common.dto.simulado.response.SimuladoExecucaoResponse;
import com.treinamaisapi.common.dto.simulado.response.SimuladoResponse;
import com.treinamaisapi.controller.swagger.SimuladoControllerSwagger;
import com.treinamaisapi.service.simulado.SimuladoService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
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





    @GetMapping("/usuario/{usuarioId}/ativo")
    @Override
    public SimuladoExecucaoResponse buscarSimuladoAtivo(@PathVariable Long usuarioId) {
        return simuladoService.buscarSimuladoAtivo(usuarioId);
    }

    // Lista histórico / todos os simulados do usuário
    @GetMapping("/usuario/{usuarioId}")
    @Override
    public List<SimuladoResponse> listarSimuladosPorUsuario(@PathVariable Long usuarioId) {
        return simuladoService.listarSimuladosPorUsuario(usuarioId);
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


}
